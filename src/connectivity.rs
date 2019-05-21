type Idx = usize;

pub struct ConnGraph {
    num_nodes: usize,
    blocks_per_line: u32,
    adj_mat: Vec<u64>,
    worklist: (Vec<Idx>, Vec<Idx>),
}

fn read_bit(block: u64, idx: Idx) -> bool {
    block & (1 << idx) != 0
}

fn write_bit(block: u64, idx: Idx) -> u64 {
    block | (1 << idx)
}

impl ConnGraph {
    pub fn new(max_size: usize) -> Self {
        assert!(max_size <= 64);
        ConnGraph {
            num_nodes: max_size,
            blocks_per_line: 1,
            adj_mat: vec![0; max_size * max_size],
            worklist: (vec![], vec![]),
        }
    }

    fn block_address(&self, a: Idx, b: Idx) -> usize {
        a * (self.blocks_per_line as usize) + b / 64
    }

    /// Returns true if no cycle was detected
    pub fn set(&mut self, a: Idx, b: Idx) -> bool {
        if a == b || self.get(b, a) {
            // there is already an edge b -> a, which mean we get a cycle.
            return false;
        }
        if self.get(a, b) {
            // this edge is already in there, do nothing
            return true;
        }
        self.write(a, b);
        //        let (is,js) = &mut self.worklist;
        let mut is = vec![];
        let mut js = vec![];
        debug_assert!(is.is_empty() && js.is_empty());

        for k in 0..self.num_nodes {
            if k == a || k == b {
                continue;
            }
            if self.get(k, a) && !self.get(k, b) {
                self.write(k, b);
                is.push(k);
            }
            if self.get(b, k) && !self.get(a, k) {
                self.write(a, k);
                js.push(k);
            }
        }
        for &i in is.iter() {
            for &j in js.iter() {
                if i != j && self.get(i, a) && self.get(a, j) {
                    self.write(i, j);
                }
            }
        }
        true
    }
    pub fn get(&self, a: usize, b: usize) -> bool {
        let block_addr = self.block_address(a, b);
        let block = self.adj_mat[block_addr];
        read_bit(block, b % 64)
    }

    fn write(&mut self, a: usize, b: usize) {
        let block_addr = self.block_address(a, b);
        let prev = self.adj_mat[block_addr];
        let new = write_bit(prev, b % 64);
        self.adj_mat[block_addr] = new;
        // bit is not set yet
    }
}
