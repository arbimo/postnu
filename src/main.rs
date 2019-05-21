#![allow(dead_code)]

extern crate structopt;
#[macro_use]
extern crate log;
extern crate env_logger;

mod algebra;
mod connectivity;
mod diff_log;
mod dist_graph;
mod labels;
mod parser;
mod postnu;

use crate::dist_graph::{build_distance_graph, propagate};
use crate::postnu::*;

use std::fs;

//use std::path::PathBuf;
use structopt::StructOpt;
use log::LevelFilter;
use std::io::Write;
use env_logger::Target;

#[derive(Debug, StructOpt)]
#[structopt(name = "example", about = "An example of StructOpt usage.")]
struct Opt {
    file: String,
    #[structopt(long = "controllable")]
    expected_controllable: Option<bool>,
    #[structopt(short = "v")]
    verbose: bool
}

fn main() {
    let opt = Opt::from_args();
    env_logger::builder()
        .filter_level(if opt.verbose { LevelFilter::Debug } else { LevelFilter::Info })
        .format(|buf, record| {
            writeln!(buf, "{}", record.args())
        })
        .target(Target::Stdout)
        .init();

    log::debug!("Options: {:?}", opt);

    let filecontent = fs::read_to_string(opt.file).expect("Cannot read file");

    let edges = crate::parser::read_all(&filecontent);
    let mut postnu = Network::<&str, i32>::new();
    edges.iter().for_each(|e| match e.tpe {
        parser::EdgeType::Observable => postnu.add_observable(
            &e.source,
            &e.target,
            e.lb.expect("Missing lb"),
            e.ub.expect("Missing ub"),
        ),
        parser::EdgeType::Hidden => postnu.add_hidden(
            &e.source,
            &e.target,
            e.lb.expect("Missing lb"),
            e.ub.expect("Missing ub"),
        ),
        parser::EdgeType::Requirement => {
            e.lb.iter()
                .for_each(|lb| postnu.add_min_delay(&e.source, &e.target, *lb));
            e.ub.iter()
                .for_each(|ub| postnu.add_max_delay(&e.source, &e.target, *ub));
        }
    });

    info!("==== Parsed POSTNU ====");
    info!("{}", postnu);
    info!("=======================");

    info!("");
    debug!("==== Hidden groups ====");
    for g in postnu.all_hidden_groups() {
        debug!("{}", g);
    }
    debug!("=======================");

    let mut dg = build_distance_graph(&postnu);
    debug!("==== Distance Graph ====");
    debug!("{}", dg);
    debug!("=======================");
    let dc = propagate(&mut dg);

    info!("\n\n==== Propagation Result ====");
    info!("Dynamically controllable : {}", dc);

    debug!("Propagated graph:\n{}",  dg);
    info!("=======================");

    match opt.expected_controllable {
        Some(x) if x != dc => {
            if x {
                eprintln!("Expected controllable but was not.");
                std::process::exit(1);
            } else {
                eprintln!("Expected uncontrollable but was.");
                std::process::exit(1);
            }
        },
        _ => ()
    }

    //    for n in ["E", "Y", "E2", "Y2"].iter() {
    //        let g = postnu.hidden_group(postnu.index_of(n));
    //
    //    }
}

#[cfg(test)]
mod tests {
    use crate::dist_graph::{build_distance_graph, propagate};
    use crate::parser;
    use crate::postnu::Network;

    fn sats() -> Vec<&'static str> {
        vec![
"X E hid 1 1
E Z req 3 3",
"X E hid 1 1
E Y obs 3 3
E Z req 3 3",
"X E hid 1 1
E Y obs 2 2
E Z req 3 4",
        ]
    }

    #[test]
    fn correct_results() {
        for filecontent in sats() {
            println!("\n=============\n\nProcessing:\n{}\n", filecontent);
            assert_sat(filecontent);
        }
    }

    fn assert_sat(filecontent: &str) {
        let edges = crate::parser::read_all(&filecontent);
        let mut postnu = Network::<&str, i32>::new();
        edges.iter().for_each(|e| match e.tpe {
            parser::EdgeType::Observable => postnu.add_observable(
                &e.source,
                &e.target,
                e.lb.expect("Missing lb"),
                e.ub.expect("Missing ub"),
            ),
            parser::EdgeType::Hidden => postnu.add_hidden(
                &e.source,
                &e.target,
                e.lb.expect("Missing lb"),
                e.ub.expect("Missing ub"),
            ),
            parser::EdgeType::Requirement => {
                e.lb.iter()
                    .for_each(|lb| postnu.add_min_delay(&e.source, &e.target, *lb));
                e.ub.iter()
                    .for_each(|ub| postnu.add_max_delay(&e.source, &e.target, *ub));
            }
        });

//        println!("{}", postnu);

        for g in postnu.all_hidden_groups() {
            println!("{}", g);
            //        let dists = g.bellman_ford(g.root);
            //        println!("{:?}", dists)
        }

        let mut dg = build_distance_graph(&postnu);
        println!("{}", dg);
        let dc = propagate(&mut dg);
        assert!(dc);
        println!("DC: ({})\n{}", dc, dg);

        //    for n in ["E", "Y", "E2", "Y2"].iter() {
        //        let g = postnu.hidden_group(postnu.index_of(n));
        //
        //    }
    }

}
