use crate::algebra::*;
use std::fmt::{Display, Error, Formatter};

#[derive(Clone, Copy, Debug)]
pub struct Wait<N, W> {
    pub node: N,
    pub delay: W,
}
#[derive(Clone, Debug)]
pub struct Lab<N, W> {
    pub root: Option<N>,
    pub scalar: W,
    pub waits: Vec<Wait<N, W>>,
}
impl<N, W> Lab<N, W>
where
    N: Ord + Copy + PartialEq,
    W: FloatLike,
{
    pub fn from_scalar(w: W) -> Self {
        Lab {
            root: None,
            scalar: w,
            waits: vec![],
        }
    }

    pub fn new(root: Option<N>, scalar: W, min_terms: &[(N, W)]) -> Self {
        Lab {
            root,
            scalar,
            waits: min_terms
                .iter()
                .map(|(n, w)| Wait {
                    node: *n,
                    delay: *w,
                })
                .collect(),
        }
    }
    pub fn new_from_vec(root: Option<N>, scalar: W, waits: Vec<Wait<N, W>>) -> Self {
        Lab {
            root,
            scalar,
            waits,
        }
    }

    pub fn as_pure_scalar(&self) -> Option<W> {
        if self.waits.is_empty() {
            Some(self.scalar)
        } else {
            None
        }
    }

    fn add_to_all(&mut self, d: W) {
        self.scalar += d;
        for e in &mut self.waits {
            e.delay += d
        }
    }

    pub fn normalize(&mut self) {
        // sort by increasing node number
        // then, for each node, sort its delays in increasing order
        self.waits.sort_by_key(|w| w.node);
    }

    fn assert_valid(&self) {
        debug_assert!(self.impl_check_valid());
    }

    fn impl_check_valid(&self) -> bool {
        let mut tmp = self.waits.clone();
        tmp.sort_by_key(|w| w.node);
        //        assert!(self.waits == tmp, "Not sorted");
        tmp.dedup_by_key(|w| w.node);
        //        assert!(self.waits == tmp, "Duplicated labels");
        true
    }

    fn compare<F>(a: &Self, b: &Self, cmp: F) -> Dominance
    where
        F: Fn(W, W) -> std::cmp::Ordering,
    {
        use std::cmp::Ordering::*;
        a.assert_valid();
        b.assert_valid();

        let mut state = Dominance::init();
        match cmp(a.scalar, b.scalar) {
            Less => state.left_win(),
            Greater => state.right_win(),
            Equal => (),
        }

        let mut ai = 0;
        let mut bi = 0;
        loop {
            println!("{} / {}", ai, bi);
            if ai == a.waits.len() && bi == b.waits.len() {
                return state;
            } else if ai == a.waits.len() {
                // ai has less disjuncts and is tighter on those
                state.left_win();
                return state;
            } else if bi == b.waits.len() {
                // ai has less disjuncts and is tighter on those
                state.right_win();
                return state;
            } else {
                let aw = a.waits[ai];
                let bw = b.waits[bi];
                if aw.node == bw.node {
                    match cmp(aw.delay, bw.delay) {
                        Less => state.left_win(),
                        Greater => state.right_win(),
                        Equal => (),
                    }
                    ai += 1;
                    bi += 1;
                } else if aw.node < bw.node {
                    // right is missing a node
                    state.right_win();
                    ai += 1;
                } else {
                    // left is missing a node
                    state.left_win();
                    bi += 1;
                }
            }
        }
    }
}
struct Dominance {
    // true if part of the left term strictly dominates the correspond right part
    // corollary: if false : left is subsumed by right
    left: bool,
    // true if part of the right term strictly dominates the correspond left part
    // corollary: if false : right is subsumed by left
    right: bool,
}
impl Dominance {
    fn init() -> Self {
        Dominance {
            left: false,
            right: false,
        }
    }
    fn left_win(&mut self) {
        self.left = true;
    }
    fn right_win(&mut self) {
        self.right = true;
    }
    pub fn identical(&self) -> bool {
        !self.left && !self.right
    }
    pub fn left_dominates(&self) -> bool {
        self.left && !self.right
    }
    pub fn right_dominates(&self) -> bool {
        !self.left && self.right
    }
    pub fn none_dominates(&self) -> bool {
        self.left && self.right
    }
}

#[derive(Clone, Debug)]
pub enum Label<N, W> {
    Min(Lab<N, W>),
    Max(Lab<N, W>),
}
impl<N, W> Label<N, W>
where
    N: Copy + Ord,
    W: FloatLike,
{
    pub fn from_scalar(w: W) -> Self {
        let ret = if w < W::zero() {
            Label::Min(Lab::from_scalar(-w))
        } else {
            Label::Max(Lab::from_scalar(w))
        };
        ret.check();
        ret
    }
    pub fn negative(root: Option<N>, scalar: W, min_terms: &[(N, W)]) -> Self {
        Label::Min(Lab::new(root, scalar, min_terms))
    }
    pub fn min(root: Option<N>, scalar: W, min_terms: Vec<Wait<N, W>>) -> Self {
        Label::Min(Lab::new_from_vec(root, scalar, min_terms))
    }

    pub fn positive(root: Option<N>, scalar: W, min_terms: &[(N, W)]) -> Self {
        Label::Max(Lab::new(root, scalar, min_terms))
    }
    pub fn max(root: Option<N>, scalar: W, min_terms: Vec<Wait<N, W>>) -> Self {
        Label::Max(Lab::new_from_vec(root, scalar, min_terms))
    }

    pub fn is_min(&self) -> bool {
        match self {
            Label::Min(_) => true,
            Label::Max(_) => false,
        }
    }
    pub fn is_max(&self) -> bool {
        !self.is_min()
    }

    pub fn tighten(&mut self) {
        match self {
            Label::Min(l) => {
                for w in &mut l.waits {
                    if w.delay < W::zero() {
                        w.delay = W::zero();
                    }
                }
            }
            Label::Max(l) => {
                l.waits.retain(|w| w.delay >= W::zero());
            }
        }
        self.check();
    }

    pub fn label(&self) -> &Lab<N, W> {
        match self {
            Label::Min(l) => l,
            Label::Max(l) => l,
        }
    }
    pub fn label_mut(&mut self) -> &mut Lab<N, W> {
        match self {
            Label::Min(l) => l,
            Label::Max(l) => l,
        }
    }

    pub fn root(&self) -> Option<N> {
        self.label().root
    }

    pub fn scalar(&self) -> W {
        self.label().scalar
    }

    pub fn add_to_all_terms(&mut self, w: W) {
        //        self.check();
        self.label_mut().add_to_all(w);
        self.tighten();
        self.check();
    }

    pub fn subsumes(&self, other: &Self) -> bool {
        match (self, other) {
            (Label::Min(la), Label::Min(lb)) => {
                let comparison = Lab::compare(la, lb, |a, b| W::cmp(&a, &b).reverse());
                comparison.identical() || comparison.left_dominates()
            }
            (Label::Max(la), Label::Max(lb)) => {
                let comparison = Lab::compare(la, lb, |a, b| W::cmp(&a, &b));
                comparison.identical() || comparison.left_dominates()
            }
            (Label::Min(_), Label::Max(_)) => true,
            (Label::Max(_), Label::Min(_)) => false,
        }
    }

    pub fn check(&self) {
        debug_assert!(self.scalar() >= W::zero());
        debug_assert!(self.is_max() || self.scalar() > W::zero());
        debug_assert!(self.label().waits.iter().all(|w| w.delay >= W::zero()));
    }
}

//impl<N, W> Display for Label<N, W>
//where
//    N: ToIndex + Copy,
//    W: Display,
//{
//    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
//        match self {
//            Label::Min(Lab {
//                root: _,
//                scalar: delay,
//                waits,
//            }) => {
//                if waits.is_empty() {
//                    write!(f, "- {}", delay)?;
//                } else {
//                    write!(f, "- min {} [ ", delay)?;
//                    for w in waits {
//                        write!(f, "({}, {}) ", w.node.to_index(), w.delay)?;
//                    }
//                    write!(f, "]")?;
//                }
//            }
//            Label::Max(Lab {
//                root: _,
//                scalar: delay,
//                waits,
//            }) => {
//                if waits.is_empty() {
//                    write!(f, "  {}", delay)?;
//                } else {
//                    write!(f, "  max {} [ ", delay)?;
//                    for w in waits {
//                        write!(f, "({}, {}) ", w.node.to_index(), w.delay)?;
//                    }
//                    write!(f, "]")?;
//                }
//            }
//        }
//        Result::Ok(())
//    }
//}

impl<N, W> Display for Label<N, W>
where
    N: Display + Copy,
    W: Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Label::Min(Lab {
                root: _,
                scalar: delay,
                waits,
            }) => {
                if waits.is_empty() {
                    write!(f, "- {}", delay)?;
                } else {
                    write!(f, "- min {} [ ", delay)?;
                    for w in waits {
                        write!(f, "({}, {}) ", w.node, w.delay)?;
                    }
                    write!(f, "]")?;
                }
            }
            Label::Max(Lab {
                root: _,
                scalar: delay,
                waits,
            }) => {
                if waits.is_empty() {
                    write!(f, "  {}", delay)?;
                } else {
                    write!(f, "  max {} [ ", delay)?;
                    for w in waits {
                        write!(f, "({}, {}) ", w.node, w.delay)?;
                    }
                    write!(f, "]")?;
                }
            }
        }
        Result::Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    type L = Label<i32, i32>;

    #[test]
    fn dominance() {
        let a: L = Label::from_scalar(1);
        let b: L = Label::from_scalar(2);
        let c: L = Label::from_scalar(-1);
        let d: L = Label::from_scalar(-2);
        assert!(a.subsumes(&b));
        assert!(a.subsumes(&a));
        assert!(!b.subsumes(&a));

        assert!(d.subsumes(&c));
        assert!(!c.subsumes(&d));

        assert!(!a.subsumes(&c));
        assert!(c.subsumes(&a));

        let p1 = Label::positive(None, 1, &[(1, 4)]);
        let p2 = Label::positive(None, 1, &[(1, 3)]);
        //        assert!(p2.subsumes(&p1));
        assert!(!p1.subsumes(&p2));

        let p1 = Label::positive(None, 1, &[(1, 4), (2, 4)]);

        // those strictly dominate p1
        let p2 = Label::positive(None, 1, &[(1, 3), (2, 4)]);
        let p3 = Label::positive(None, 1, &[(1, 4), (2, 3)]);
        let p4 = Label::positive(None, 1, &[(1, 4)]);
        let p5 = Label::positive(None, 0, &[(1, 4), (2, 4)]);

        for (i, other) in [p2, p3, p4, p5].iter().enumerate() {
            assert!(other.subsumes(&p1), "p{}", i + 2);
            assert!(!p1.subsumes(&other), "!(p1 subsumes p{})", i + 2);
        }

        // none dominate the other
        let p12 = Label::positive(None, 0, &[(1, 5), (2, 4)]);
        let p13 = Label::positive(None, 0, &[(1, 5), (2, 4), (3, 2)]);
        for (i, other) in [p12, p13].iter().enumerate() {
            assert!(!other.subsumes(&p1), "{}", i);
            assert!(!p1.subsumes(&other), "{}", i);
        }

        let p1 = Label::negative(None, 1, &[(1, 4), (2, 4)]);

        // those strictly dominate p1
        let p2 = Label::negative(None, 1, &[(1, 3), (2, 4)]);
        let p3 = Label::negative(None, 1, &[(1, 4), (2, 3)]);
        let p4 = Label::negative(None, 1, &[(1, 4), (2, 4), (3, 4)]);
        let p5 = Label::negative(None, 0, &[(1, 4), (2, 4)]);

        for (i, other) in [p2, p3, p4, p5].iter().enumerate() {
            assert!(!other.subsumes(&p1), "p{}", i + 2);
            assert!(p1.subsumes(&other), "!(p1 subsumes p{})", i + 2);
        }
    }
}
