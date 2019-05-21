use log::{debug};

use crate::labels::*;

use crate::algebra::FloatLike;
use crate::connectivity::ConnGraph;
use crate::diff_log::*;
use crate::dist_graph::Node::*;
use crate::postnu::*;
use petgraph::prelude::NodeIndex;
use petgraph::stable_graph::StableDiGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Error, Formatter};
use std::hash::Hash;

type NI = NodeIndex<u32>;

#[derive(Debug)]
enum Node<N> {
    Contr(N),
    Obs(N),
    HidLow(N),
    HidHigh(N),
}

#[derive(Debug)]
struct Split<N, W> {
    low: N,
    high: N,
    alpha: W,
    beta: W,
}
impl<N, W> Split<N, W>
where
    N: Copy + PartialEq,
    W: Copy + FloatLike,
{
    pub fn of_non_hidden(n: N) -> Self {
        Split {
            low: n,
            high: n,
            alpha: W::zero(),
            beta: W::zero(),
        }
    }

    fn as_single_node(&self) -> N {
        assert!(self.low == self.high);
        self.low
    }
}

#[derive(Debug)]
pub struct DistanceGraph<N, W>
where
    N: Eq + Hash,
{
    dict: HashMap<N, Split<NI, W>>,
    graph: StableDiGraph<Node<N>, Label<NI, W>>,
}
impl<N, W> DistanceGraph<N, W>
where
    N: Copy + Eq + Hash,
    W: Copy,
{
    pub fn new() -> Self {
        DistanceGraph {
            dict: HashMap::new(),
            graph: StableDiGraph::new(),
        }
    }

    pub fn low(&self, n: N) -> NI {
        self.dict[&n].low
    }
    pub fn high(&self, n: N) -> NI {
        self.dict[&n].high
    }
    pub fn alpha(&self, n: N) -> W {
        self.dict[&n].alpha
    }
    pub fn beta(&self, n: N) -> W {
        self.dict[&n].beta
    }
}

impl<N: Eq + Hash + Debug, W: Display> Display for DistanceGraph<N, W> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Nodes:\n")?;
        for n in self.graph.node_indices() {
            write!(
                f,
                "  {}: {:?}\n",
                n.index(),
                self.graph.node_weight(n).unwrap()
            )?;
        }
        write!(f, "Edges:\n")?;
        for n in self.graph.node_indices() {
            for e in self.graph.edges_directed(n, Direction::Outgoing) {
                write!(
                    f,
                    "  {} -> {} : {}\n",
                    e.source().to_index(),
                    e.target().to_index(),
                    e.weight()
                )?;
            }
        }
        Result::Ok(())
    }
}

pub trait ToIndex {
    fn to_index(self) -> usize;
}

impl ToIndex for NI {
    fn to_index(self) -> usize {
        self.index()
    }
}

pub fn build_distance_graph<N, W>(postnu: &Network<N, W>) -> DistanceGraph<N, W>
where
    N: Eq + Hash + Copy + Ord + Display,
    W: FloatLike,
{
    debug!("\n\n==== Building distance graph ====");
    let mut dist_graph: DistanceGraph<N, W> = DistanceGraph::new();

    // add controllable and observable nodes
    for ni in postnu.graph.node_indices() {
        let n = postnu.label_of(ni);
        let tpe = postnu.type_of(ni);
        match tpe {
            TPType::Controllable => {
                let id = dist_graph.graph.add_node(Node::Contr(n));
                dist_graph.dict.insert(n, Split::of_non_hidden(id));
            }
            TPType::Observable => {
                let id = dist_graph.graph.add_node(Node::Obs(n));
                dist_graph.dict.insert(n, Split::of_non_hidden(id));
            }
            TPType::Hidden => {
                // processed later since we need the alpha/beta information
            }
        }
    }

    for hgroup in postnu.all_hidden_groups().iter() {
        let mut vertices = Vec::new();
        hgroup.eyes.iter().for_each(|e| vertices.push(e));
        hgroup.hiddens.iter().for_each(|e| vertices.push(e));

        for &ni in vertices {
            let dists = hgroup
                .bellman_ford(ni)
                .expect("Hidden group with negative cycle");
            let lab = |i| *postnu.graph.node_weight(i).unwrap();
            let ix = |i| dist_graph.dict[&lab(i)].as_single_node();

            debug!("\nProcessing node: {}  (named {})", ni.index(), postnu.graph.node_weight(ni).unwrap());

            let mut l_hi = Label::min(
                Some(ix(hgroup.root)),
                -dists.from(hgroup.root),
                hgroup
                    .eyes
                    .iter()
                    .map(|&i| Wait {
                        node: ix(i),
                        delay: -dists.from(i),
                    })
                    .collect::<Vec<_>>(),
            );
            debug!("l_hi: {}", l_hi);
            let alpha = -l_hi.scalar().min(
                l_hi.label()
                    .waits
                    .iter()
                    .map(|w| w.delay)
                    .min()
                    .unwrap_or(W::zero()),
            );
            debug!("alpha: {}", alpha);

            l_hi.add_to_all_terms(alpha);
            let l_hi = l_hi; // hide by immutable
            debug!("l_hi+: {}", l_hi);
            let mut l_lo = Label::max(
                Some(ix(hgroup.root)),
                -dists.to(hgroup.root),
                hgroup
                    .eyes
                    .iter()
                    .map(|&i| Wait {
                        node: ix(i),
                        delay: -dists.to(i),
                    })
                    .collect(),
            );
            debug!("l_lo: {}", l_lo);
            let beta = -l_lo.scalar().min(
                l_lo.label()
                    .waits
                    .iter()
                    .map(|w| w.delay)
                    .min()
                    .unwrap_or(W::zero()),
            );
            debug!("beta: {}", beta);

            l_lo.add_to_all_terms(beta);
            l_lo.tighten();
            debug!("l_lo+: {}", l_lo);
            let l_lo = l_lo;

            let lab_e = lab(ni);
            let ix_root = ix(hgroup.root);

            if postnu.type_of(ni) == TPType::Hidden {
                let id_lo = dist_graph.graph.add_node(HidLow(lab_e));
                let id_hi = dist_graph.graph.add_node(HidHigh(lab_e));

                dist_graph.dict.insert(
                    lab(ni),
                    Split {
                        low: id_lo,
                        high: id_hi,
                        alpha,
                        beta,
                    },
                );
            }
            let id_lo = dist_graph.dict[&lab(ni)].low;
            let id_hi = dist_graph.dict[&lab(ni)].high;

            dist_graph.graph.add_edge(ix_root, id_lo, l_lo);
            dist_graph.graph.add_edge(id_hi, ix_root, l_hi);
        }
    }

    for req in postnu.graph.edge_references() {
        match req.weight() {
            Link::MaxDelay(d) => {
                let l_a = postnu.label_of(req.source());
                let l_b = postnu.label_of(req.target());


                let a = dist_graph.low(l_a);
                let b = dist_graph.high(l_b);
                let l = *d - dist_graph.beta(l_a) + dist_graph.alpha(l_b);
                debug!("req: {} -> {} : {}   (before delaying: {})", a.index(), b.index(), l, *d);

                dist_graph.graph.add_edge(a, b, Label::from_scalar(l));
            }
            _ => (),
        }
    }
    debug!("=======================\n");
    dist_graph
}

pub fn reduce<N, W>(ab: &Label<N, W>, bc: &Label<N, W>) -> Result<Option<Label<N, W>>, ()>
where
    N: PartialEq + Ord + Copy + ToIndex + Debug,
    W: FloatLike + Debug,
{
    debug_assert!(ab.is_max());
    debug_assert!(bc.is_min());
    let u0 = ab.scalar();
    let v0 = bc.scalar();

    if ab.root().is_some() && ab.root() == bc.root() {
        let root = ab.root().unwrap();
        let mins = {
            let mut tmp = bc.label().waits.clone();
            tmp.push(Wait {
                node: root,
                delay: bc.scalar(),
            });
            tmp
        };
        let maxs = {
            let mut tmp = ab.label().waits.clone();
            tmp.push(Wait {
                node: root,
                delay: ab.scalar(),
            });
            tmp
        };
        let mut diffs = Differences::<N, W>::new(root);
        for min_term in &mins {
            for max_term in &maxs {
                diffs.add_diff_strict_bound(
                    max_term.node,
                    min_term.node,
                    min_term.delay - max_term.delay,
                );
            }
        }
        match diffs.assignment() {
            Some(_) => {
                // there is a projection where we have a negative self loop
                Result::Err(())
            }
            None => {
                // the combination is consistent but useless (a positive self loop)
                Result::Ok(None)
            }
        }
    } else if u0 < v0 {
        // diff root / negative
        let mut ac = bc.clone();
        ac.add_to_all_terms(-u0);
        ac.tighten();
        Result::Ok(Some(ac))
    } else {
        // different root / positive
        let mut ac = ab.clone();
        ac.add_to_all_terms(-v0);
        ac.tighten();
        Result::Ok(Some(ac))
    }
}


pub fn propagate<N, W>(dg: &mut DistanceGraph<N, W>) -> bool
where
    N: PartialEq + Ord + Copy + Hash,
    W: FloatLike + Debug,
{
    debug!("\n\n==== Propagating ====");
    let g = &mut dg.graph;
    let mut queue = vec![];
    for e in g.edge_indices() {
        // add all negative edges in the original graph
        // this will still trigger all reductions since they all involve exactly one negative edge
        if g.edge_weight(e).iter().all(|w| w.is_min()) {
            queue.push(e);
        }
    }

    let mut cycles = ConnGraph::new(10);

    while !queue.is_empty() {
        let e = queue.pop().unwrap();

        let mut reduction_queue = vec![];

        match g.edge_weight(e).unwrap() {
            Label::Min(_) => {
                let (b, _c) = g.edge_endpoints(e).unwrap();
                for ab in g.edges_directed(b, Direction::Incoming) {
                    if ab.weight().is_min() {
                        continue;
                    }
                    debug_assert!(ab.target() == b);
                    reduction_queue.push((ab.id(), e));

                }
            }
            Label::Max(_) => {
                let (_a, b) = g.edge_endpoints(e).unwrap();
                for bc in g.edges_directed(b, Direction::Outgoing) {
                    if bc.weight().is_max() {
                        continue;
                    }
                    debug_assert!(bc.source() == b);
                    reduction_queue.push((e, bc.id()));
                }
            }
        }
        for (ab, bc) in reduction_queue.drain(..) {

            let a = g.edge_endpoints(ab).unwrap().0;
            let (b,c) = g.edge_endpoints(bc).unwrap();
            let ab_weight = g.edge_weight(ab).unwrap();
            let bc_weight = g.edge_weight(bc).unwrap();

            debug!("\nReducing: {} -> {} -> {}", a.index(), b.index(), c.index());
            debug!("labels: {}     {}", ab_weight, bc_weight);

            let opt_ac_label = reduce(ab_weight, bc_weight);
                    match opt_ac_label {
                        Result::Ok(Some(lab)) => {
                            debug!("diff-root cross case.");
                            // new label derived
                            debug!("New label derived {} -> {}: {}", a.index(), c.index(), lab);

                            let dominator = g
                                .edges_directed(a, Direction::Outgoing)
                                .filter(|e| e.target() == c)
                                .find(|e| e.weight().subsumes(&lab));
                            match dominator {
                                None => {
                                    debug!("Not dominated, add to graph");
                                    let edge_id = g.add_edge(a, c, lab.clone());
                                    queue.push(edge_id);
                                    if lab.is_min() {
                                        // this edge is negative
                                        if !cycles.set(a.index(), c.index()) {
                                            // cycle detected
                                            debug!("Detected cycle");
                                            return false;
                                        }
                                    }
                                },
                                Some(d) => {
                                    debug!("Dominated by existing: {}", d.weight());
                                }
                            }

                        },
                        Result::Ok(None) => {
                            debug!("Same-root cross case: ok");
                        },
                        Result::Err(_) => {
                            debug!("Same-root cross case: Negative self edge in projection");
                            return false;
                        }
                    }
        }
    }
    debug!("quiescence");
    debug!("===================\n");

    true
}
