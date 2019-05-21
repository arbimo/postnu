use crate::algebra::FloatLike;
use crate::dist_graph::ToIndex;
use crate::postnu::Distances;

#[derive(Debug)]
pub struct Differences<N, W> {
    zero: N,
    constraints: Vec<(N, N, W)>,
}
impl<N, W> Differences<N, W>
where
    N: Copy + ToIndex,
    W: FloatLike,
{
    pub fn new(zero: N) -> Self {
        Differences {
            zero,
            constraints: vec![],
        }
    }

    pub fn add_diff_bound(&mut self, a: N, b: N, ub: W) {
        self.constraints.push((a, b, ub));
    }
    pub fn add_diff_strict_bound(&mut self, a: N, b: N, ub: W) {
        self.add_diff_bound(a, b, ub - W::epsilon());
    }

    pub fn assignment(&self) -> Option<Distances<N, W>> {
        let mut biggest: usize = 0;
        for c in self.constraints.iter() {
            biggest = biggest.max(c.0.to_index().max(c.1.to_index()));
        }
        let n = biggest + 1;

        let mut distances = Distances::<N, W>::new(self.zero, n);
        let d = &mut distances.dists;

        let mut updated = false;
        for _ in 0..n + 1 {
            updated = false;
            for md in self.constraints.iter() {
                let s: usize = md.0.to_index();
                let e: usize = md.1.to_index();
                let w = md.2;
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
        //            false
        } else {
            // finished in at most n iterations
            Some(distances)
            //            true
        }
    }
}
