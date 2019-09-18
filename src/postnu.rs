use crate::algebra::FloatLike;

use crate::dist_graph::ToIndex;
use petgraph::graph::IndexType;
use petgraph::prelude::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::{Direction, Graph};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Neg;

pub enum Link<W> {
    MaxDelay(W),
    Observable(W, W),
    Hidden(W, W),
}
impl<W> Link<W> {
    pub fn is_contingent(&self) -> bool {
        match self {
            Link::MaxDelay(_) => false,
            _ => true,
        }
    }
}

#[derive(PartialEq,Debug)]
pub enum TPType {
    Controllable,
    Observable,
    Hidden,
}

//type Network<N,W> = s3graph::DiGraph<N,Link<W>>;

pub struct Network<N, W> {
    pub nodes: HashMap<N, NI>,
    pub graph: Graph<N, Link<W>>,
}

impl<N: Eq + Hash + Copy, W> Network<N, W> {
    pub fn new() -> Self {
        Network {
            nodes: HashMap::new(),
            graph: Graph::new(),
        }
    }

    fn record(&mut self, n: N) -> NI {
        if !self.nodes.contains_key(&n) {
            let id = self.graph.add_node(n);
            self.nodes.insert(n, id);
        }
        self.index_of(n)
    }
    pub fn index_of(&self, n: N) -> NI {
        self.nodes[&n]
    }
    pub fn label_of(&self, ni: NI) -> N {
        *self.graph.node_weight(ni).unwrap()
    }
}

impl<N: fmt::Display + Eq + Hash + Copy, W: fmt::Display> fmt::Display for Network<N, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Nodes:\n")?;
        for ni in self.graph.node_indices() {
            write!(f, "  {}: {}\n", ni.index(), self.label_of(ni))?;
        }
        write!(f, "Edges:\n")?;
        for ni in self.graph.node_indices() {
            for e in self.graph.edges_directed(ni, Direction::Outgoing) {
                write!(f, "  {} -> {}  ", e.source().index(), e.target().index())?;
                match e.weight() {
                    Link::MaxDelay(max) => write!(f, "max-delay: {}", *max)?,
                    Link::Hidden(lb, ub) => write!(f, "hidden: [{}, {}]", *lb, *ub)?,
                    Link::Observable(lb, ub) => write!(f, "observable: [{}, {}]", *lb, *ub)?,
                }
                writeln!(f, "")?;
            }
        }
        Result::Ok(())
    }
}

impl<N: Eq + Hash + Copy, W: Copy + Neg<Output = W>> Network<N, W> where {
    pub fn add_observable(&mut self, source: N, target: N, lb: W, ub: W) {
        let a = self.record(source);
        let b = self.record(target);
        self.graph.add_edge(a, b, Link::Observable(lb, ub));
    }
    pub fn add_hidden(&mut self, source: N, target: N, lb: W, ub: W) {
        let a = self.record(source);
        let b = self.record(target);
        self.graph.add_edge(a, b, Link::Hidden(lb, ub));
    }

    pub fn type_of(&self, n: NI) -> TPType {
        for e in self.graph.edges_directed(n, Direction::Incoming) {
            match e.weight() {
                Link::MaxDelay(_) => (),
                Link::Observable(_, _) => return TPType::Observable,
                Link::Hidden(_, _) => return TPType::Hidden,
            }
        }
        // no contingent links, timepoint is controllable
        TPType::Controllable
    }

    pub fn add_min_delay(&mut self, from: N, to: N, min_delay: W) {
        self.add_max_delay(to, from, -min_delay);
    }
    pub fn add_max_delay(&mut self, from: N, to: N, max_delay: W) {
        let a = self.record(from);
        let b = self.record(to);
        self.graph.add_edge(a, b, Link::MaxDelay(max_delay));
    }
    pub fn add_min_max_delay(&mut self, from: N, to: N, min_delay: W, max_delay: W) {
        self.add_min_delay(from, to, min_delay);
        self.add_max_delay(from, to, max_delay);
    }

    fn incoming_contingent(&self, n: NI) -> Option<(NI, W, W)> {
        self.graph
            .edges_directed(n, Direction::Incoming)
            .filter(|e| e.weight().is_contingent())
            .next()
            .map(|l| -> (NI, W, W) {
                match l.weight() {
                    Link::Observable(lb, ub) => (l.source(), *lb, *ub),
                    Link::Hidden(lb, ub) => (l.source(), *lb, *ub),
                    _ => panic!(),
                }
            })
    }

    /// Returns the earliest hidden timepoint that is part of the same hidden group as n
    /// The result is None if there is no hidden timepoint in the hidden group of n.
    /// The function will panic if n is controllable
    ///
    /// Note : two timepoints are in the same hidden group iff the have the same hidden root
    fn hidden_root(&self, n: NI) -> Option<NI> {
        let earlier_hidden = match self.incoming_contingent(n) {
            None => panic!(),
            Some((trigger, _, _)) => match self.type_of(trigger) {
                TPType::Hidden => self.hidden_root(trigger),
                _ => None,
            },
        };
        earlier_hidden.or(match self.type_of(n) {
            TPType::Controllable => panic!(),
            TPType::Hidden => Some(n),
            TPType::Observable => None,
        })
    }

    pub fn hidden_group(&self, from: NI) -> HiddenGroup<NI, W> {
        match self.hidden_root(from) {
            None => {
                let (root_obs, from_root_lb, from_root_ub) =
                    self.incoming_contingent(from).unwrap();
                return HiddenGroup {
                    root: root_obs,
                    eyes: vec![from],
                    hiddens: vec![],
                    dists: vec![
                        min_delay(root_obs, from, from_root_lb),
                        max_delay(root_obs, from, from_root_ub),
                    ],
                };
            }
            Some(hroot) => {
                let mut queue = vec![hroot];
                let (root_obs, from_root_lb, from_root_ub) =
                    self.incoming_contingent(hroot).unwrap();
                let mut group = HiddenGroup {
                    root: root_obs,
                    eyes: vec![],
                    hiddens: vec![hroot],
                    dists: vec![
                        min_delay(root_obs, hroot, from_root_lb),
                        max_delay(root_obs, hroot, from_root_ub),
                    ],
                };

                while !queue.is_empty() {
                    let cur_id = queue.pop().unwrap();
                    assert!(self.type_of(cur_id) == TPType::Hidden);
                    for e in self.graph.edges_directed(cur_id, Direction::Outgoing) {
                        let target = e.target();
                        match e.weight() {
                            Link::MaxDelay(_) => (),
                            Link::Hidden(lb, ub) => {
                                // the target is hidden, add it to the queue
                                queue.push(target);
                                group.hiddens.push(target);
                                // add distances to the NatureSTN
                                group.dists.push(min_delay(cur_id, target, *lb));
                                group.dists.push(max_delay(cur_id, target, *ub));
                            }
                            Link::Observable(lb, ub) => {
                                // no other nodes in the hidden group after this one
                                // simply add distances to nature STN
                                group.eyes.push(target);
                                group.dists.push(min_delay(cur_id, target, *lb));
                                group.dists.push(max_delay(cur_id, target, *ub));
                            }
                        }
                    }
                }
                group
            }
        }
    }

    pub fn all_hidden_groups(&self) -> Vec<HiddenGroup<NI, W>> {
        // nodes from which to start to explore the different hidden groups.
        // We project each contingent to its hidden root if it has one.
        // The resulting set is guaranteed to have one contingent per hidden group.
        let hidden_roots: HashSet<_> = self
            .graph
            .node_indices()
            .filter(|ni| self.type_of(*ni) != TPType::Controllable)
            .map(|ni| self.hidden_root(ni).unwrap_or(ni))
            .collect();

        hidden_roots
            .iter()
            .map(|hr| self.hidden_group(*hr))
            .collect()
    }
}

#[derive(Debug)]
pub struct MinDelay<N, W> {
    from: N,
    to: N,
    delay: W,
}

impl<N, W> MinDelay<N, W> {
    pub fn new(from: N, to: N, delay: W) -> Self {
        MinDelay { from, to, delay }
    }
}
fn min_delay<N, W>(from: N, to: N, delay: W) -> MinDelay<N, W> {
    MinDelay::new(from, to, delay)
}
fn max_delay<N, W: Neg<Output = W>>(from: N, to: N, delay: W) -> MinDelay<N, W> {
    min_delay(to, from, -delay)
}

type NI = NodeIndex<u32>;

#[derive(Debug)]
pub struct HiddenGroup<N, W> {
    pub root: N,
    pub eyes: Vec<N>,
    pub hiddens: Vec<N>,
    pub dists: Vec<MinDelay<N, W>>,
}

#[derive(Clone, Debug)]
pub struct Dist<W> {
    pub forward: W,
    pub backward: W,
}
impl<W: FloatLike> Dist<W> {
    fn default() -> Self {
        Dist {
            forward: W::infty(),
            backward: W::neg_infty(),
        }
    }

    fn zero() -> Self {
        Dist {
            forward: W::zero(),
            backward: W::zero(),
        }
    }
}

#[derive(Debug)]
pub struct Distances<N, W> {
    pub dists: Vec<Dist<W>>,
    node: PhantomData<N>,
}
impl<N: ToIndex, W: FloatLike> Distances<N, W> {
    pub fn new(source: N, n: usize) -> Self {
        let mut dists = vec![Dist::default(); n];
        dists[source.to_index()] = Dist::zero();
        Distances {
            dists,
            node: PhantomData,
        }
    }

    pub fn to(&self, ni: NI) -> W {
        self.dists[ni.to_index()].forward
    }
    pub fn from(&self, ni: NI) -> W {
        self.dists[ni.to_index()].backward
    }
}

impl<W: FloatLike> HiddenGroup<NI, W> {
    pub fn bellman_ford(&self, source: NI) -> Option<Distances<NI, W>> {
        let mut biggest: usize = self.root.index();
        for n in self.eyes.iter() {
            biggest = biggest.max((*n).index());
        }
        for n in self.hiddens.iter() {
            biggest = biggest.max((*n).index());
        }
        let n = biggest + 1;

        let mut distances = Distances::<_, W>::new(source, n);
        let d = &mut distances.dists;

        let mut updated = false;
        for _ in 0..n + 1 {
            updated = false;
            for md in self.dists.iter() {
                let s: usize = md.to.index();
                let e: usize = md.from.index();
                let w = -md.delay;
                if d[e].forward > d[s].forward + w {
                    d[e].forward = d[s].forward + w;
                    updated = true;
                }
                if d[s].backward < d[e].backward - w {
                    d[s].backward = d[e].backward - w;
                    updated = true;
                }
            }
            if !updated {
                // exit early if distances where not updated in this iteration
                break;
            }
        }
        if updated {
            // distances updated in the last iteration
            // there is a negative cycle
            None
        } else {
            // finished in at most n iterations
            Some(distances)
        }
    }
}

impl<X: IndexType, W: Copy + Display + Neg<Output = W>> fmt::Display
    for HiddenGroup<NodeIndex<X>, W>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Hidden group:\n",)?;
        write!(f, "  root: {}\n", self.root.index())?;
        write!(
            f,
            "  eyes: {:?} \n",
            self.eyes.iter().map(|n| n.index()).collect::<Vec<_>>()
        )?;
        write!(
            f,
            "  hidden: {:?} \n",
            self.hiddens.iter().map(|n| n.index()).collect::<Vec<_>>()
        )?;
        write!(f, "  dists:\n")?;
        for md in &self.dists {
            write!(
                f,
                "    {} -- {} --> {}\n",
                md.to.index(),
                -md.delay,
                md.from.index()
            )?;
        }
        Result::Ok(())
    }
}
