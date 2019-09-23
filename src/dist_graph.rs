use log::debug;

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
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Error, Formatter};
use std::hash::Hash;

type NI = NodeIndex<u32>;

#[derive(Debug)]
enum Node<N> {
    Contr(N),
    CtgLow(N),
    CtgHigh(N),
}

#[derive(Debug)]
struct Split<N, W> {
    low: N,
    high: N,
    delay_lo: W,
    delay_hi: W,
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
            delay_lo: W::zero(),
            delay_hi: W::zero(),
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
    graph: StableDiGraph<Node<N>, Label<N, W>>,
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
    pub fn delay_low(&self, n: N) -> W {
        self.dict[&n].delay_lo
    }
    pub fn delay_high(&self, n: N) -> W {
        self.dict[&n].delay_hi
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
                    "  {} -> {} : ??\n", // TODO : display edge.
                    e.source().to_index(),
                    e.target().to_index(),
                    //                    e.weight()
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

impl ToIndex for usize {
    fn to_index(self) -> usize {
        self
    }
}

pub fn build_distance_graph<N, W>(postnu: &Network<N, W>) -> DistanceGraph<N, W>
where
    N: Eq + Hash + Copy + Ord + Display,
    W: FloatLike,
{
    debug!("\n\n==== Building distance graph ====");
    let mut dist_graph: DistanceGraph<N, W> = DistanceGraph::new();

    // add controllable nodes
    for ni in postnu.graph.node_indices() {
        let n = postnu.label_of(ni);
        let tpe = postnu.type_of(ni);
        match tpe {
            TPType::Controllable => {
                let id = dist_graph.graph.add_node(Node::Contr(n));
                dist_graph.dict.insert(n, Split::of_non_hidden(id));
            }
            _ => (), // processed later since we need the delay information information
        }
    }

    let hidden_groups = postnu.all_hidden_groups();
    let mut queue: VecDeque<_> = hidden_groups.iter().collect();

    // function lab(i) returns the label of i where i is the the node index in POSTNU graph
    let name_of = |i| {
        *postnu
            .graph
            .node_weight(i)
            .expect("Node is not yet in postnu.graph")
    };

    let mut cnt = 0; // counter to fail on infinite loop

    while !queue.is_empty() {
        // select the next hidden group such that its root has been processed
        let hgroup = match queue.pop_front() {
            Some(x) => {
                if dist_graph.dict.contains_key(&name_of(x.root)) {
                    // the root has been recorded; keep processing the hidden group
                    x
                } else {
                    // root not processes
                    queue.push_back(x);
                    cnt += 1;
                    assert!(
                        cnt < 100000,
                        "A LOT of repetitions, suspected circular dependency."
                    );

                    continue;
                }
            }
            _ => panic!("Unexpected empty queue."),
        };

        // build list of vertices to process
        // this includes all eyes and hidden timepoints
        // the root is excluded since it is either controllable (and already in the graph)
        // or observable (and thus the eye of another hidden group)
        let mut vertices = Vec::new();
        hgroup.eyes.iter().for_each(|v| vertices.push(v));
        hgroup.hiddens.iter().for_each(|v| vertices.push(v));

        for &ni in vertices {
            let dists = hgroup
                .bellman_ford(ni)
                .expect("Hidden group with negative cycle");
            let name = name_of(ni);

            debug!(
                "\nProcessing node: {}  (named {})",
                ni.index(),
                postnu.graph.node_weight(ni).unwrap()
            );

            let mut l_hi = Label::min(
                Some(name_of(hgroup.root)),
                -dists.from(hgroup.root),
                hgroup
                    .eyes
                    .iter()
                    .map(|&i| Wait {
                        node: name_of(i),
                        delay: -dists.from(i),
                    })
                    .collect::<Vec<_>>(),
            );
            let delay_hi = -l_hi.scalar().min(
                l_hi.label()
                    .waits
                    .iter()
                    .map(|w| w.delay)
                    .min()
                    .unwrap_or(W::zero()),
            );

            l_hi.add_to_all_terms(delay_hi);
            let l_hi = l_hi; // hide by immutable
            let mut l_lo = Label::<N, W>::max(
                Some(name_of(hgroup.root)),
                -dists.to(hgroup.root),
                hgroup
                    .eyes
                    .iter()
                    .map(|&i| Wait {
                        node: name_of(i),
                        delay: -dists.to(i),
                    })
                    .collect(),
            );
            let delay_lo = -l_lo.scalar().min(
                l_lo.label()
                    .waits
                    .iter()
                    .map(|w| w.delay)
                    .min()
                    .unwrap_or(W::zero()),
            );

            l_lo.add_to_all_terms(delay_lo);
            l_lo.tighten();
            let l_lo = l_lo;

            let root = name_of(hgroup.root);

            assert_ne!(postnu.type_of(ni), TPType::Controllable);

            debug!(
            "name: {}\n  -- observable root: {}\n  --  LO  delay: {}  --  l_lo+: {}\n  --  HI  delay: {}  --  l_hi+: {}",
            name, root, delay_lo, l_lo, delay_hi, l_hi
        );

            let root_lo = dist_graph.low(root);
            let root_hi = dist_graph.high(root);
            assert_eq!(
                root_lo, root_hi,
                "The algorithm has not been proved correct when the root is split"
            );

            if postnu.type_of(ni) != TPType::Controllable {
                let id_lo = dist_graph.graph.add_node(CtgLow(name));
                let id_hi = dist_graph.graph.add_node(CtgHigh(name));

                dist_graph.dict.insert(
                    name_of(ni),
                    Split {
                        low: id_lo,
                        high: id_hi,
                        delay_lo,
                        delay_hi,
                    },
                );
            }
            let id_lo = dist_graph.dict[&name].low;
            let id_hi = dist_graph.dict[&name].high;

            dist_graph.graph.add_edge(root_lo, id_lo, l_lo);
            dist_graph.graph.add_edge(id_hi, root_hi, l_hi);
        }
    }

    for req in postnu.graph.edge_references() {
        match req.weight() {
            Link::MaxDelay(d) => {
                let l_a = postnu.label_of(req.source());
                let l_b = postnu.label_of(req.target());

                let a = dist_graph.low(l_a);
                let b = dist_graph.high(l_b);
                let l = *d - dist_graph.delay_low(l_a) + dist_graph.delay_high(l_b);
                debug!(
                    "req: {} -> {} : {}   (before delaying: {})",
                    a.index(),
                    b.index(),
                    l,
                    *d
                );

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
    N: PartialEq + Ord + Copy + Debug + Hash,
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
        // map that assigns a numeric ID to each node name
        let mut id_map = HashMap::new();
        for x in mins.iter().chain(maxs.iter()) {
            if !id_map.contains_key(&x.node) {
                id_map.insert(&x.node, id_map.len());
            }
        }
        let mut diffs = Differences::new(id_map[&root]);
        for min_term in &mins {
            for max_term in &maxs {
                diffs.add_diff_strict_bound(
                    id_map[&max_term.node],
                    id_map[&min_term.node],
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
    N: PartialEq + Ord + Copy + Hash + Debug,
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
            let (b, c) = g.edge_endpoints(bc).unwrap();
            let ab_weight = g.edge_weight(ab).unwrap();
            let bc_weight = g.edge_weight(bc).unwrap();

            debug!(
                "\nReducing: {} -> {} -> {}",
                a.index(),
                b.index(),
                c.index()
            );
            debug!("labels: {:?}     {:?}", ab_weight, bc_weight);

            let opt_ac_label = reduce(ab_weight, bc_weight);
            match opt_ac_label {
                Result::Ok(Some(lab)) => {
                    debug!("diff-root cross case.");
                    // new label derived
                    debug!(
                        "New label derived {} -> {}: {:?}",
                        a.index(),
                        c.index(),
                        lab
                    );

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
                        }
                        Some(d) => {
                            debug!("Dominated by existing: {:?}", d.weight()); //TODO: use fmt::Display
                        }
                    }
                }
                Result::Ok(None) => {
                    debug!("Same-root cross case: ok");
                }
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
