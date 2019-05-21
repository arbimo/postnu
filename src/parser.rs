use std::fmt::Error;

#[derive(PartialEq, Debug)]
pub enum EdgeType {
    Requirement,
    Observable,
    Hidden,
}

#[derive(PartialEq, Debug)]
pub struct Edge<N, W> {
    pub source: N,
    pub target: N,
    pub tpe: EdgeType,
    pub lb: Option<W>,
    pub ub: Option<W>,
}
impl<N, W> Edge<N, W> {
    pub fn req(source: N, target: N, lb: Option<W>, ub: Option<W>) -> Edge<N, W> {
        Edge {
            source,
            target,
            tpe: EdgeType::Requirement,
            lb,
            ub,
        }
    }
}

//type ParseErr = std::option::NoneError;

trait RW
where
    Self: std::marker::Sized,
{
    fn read(input: &str) -> Self;
    fn write<F: std::fmt::Write>(f: &mut F, input: &Self) -> Result<(), Error>;
}

impl RW for Edge<String, i32> {
    fn read(input: &str) -> Self {
        let mut parts = input.split_whitespace();
        let source = parts.next().unwrap().to_string();
        let target = parts.next().unwrap().to_string();
        let tpe = match parts.next().unwrap() {
            "req" => EdgeType::Requirement,
            "hid" => EdgeType::Hidden,
            "obs" => EdgeType::Observable,
            _ => panic!(),
        };
        let lb: Option<i32> = match parts.next().unwrap() {
            "_" => None,
            i => Some(i.parse().unwrap()),
        };
        let ub: Option<i32> = match parts.next().unwrap() {
            "_" => None,
            i => Some(i.parse().unwrap()),
        };
        Edge {
            source,
            target,
            tpe,
            lb,
            ub,
        }
    }

    fn write<F: std::fmt::Write>(f: &mut F, input: &Self) -> Result<(), Error> {
        write!(f, "{} {}", input.source, input.target)?;
        write!(
            f,
            " {}",
            match input.tpe {
                EdgeType::Requirement => "req",
                EdgeType::Observable => "obs",
                EdgeType::Hidden => "hid",
            }
        )?;
        for n in &[input.lb, input.ub] {
            match n {
                None => write!(f, " _")?,
                Some(i) => write!(f, " {}", i)?,
            };
        }
        Result::Ok(())
    }
}

pub fn read_all(input: &str) -> Vec<Edge<String, i32>> {
    input
        .lines()
        .filter(|l| !l.is_empty() && l.chars().next() != Some('#'))
        .map(|l| Edge::read(l))
        .collect()
}

pub fn write_all<F: std::fmt::Write>(
    f: &mut F,
    input: &Vec<Edge<String, i32>>,
) -> Result<(), Error> {
    for e in input {
        Edge::write(f, e)?;
    }
    Result::Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_lines() {
        let e = Edge::read("A B req 0 5");
        assert_eq!(
            e,
            Edge::req("A".to_string(), "B".to_string(), Some(0), Some(5))
        );

        let e = Edge::read("A B req _ -5");
        assert_eq!(
            e,
            Edge::req("A".to_string(), "B".to_string(), None, Some(-5))
        );
    }
}
