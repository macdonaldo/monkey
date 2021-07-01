


pub struct Defers {
    stack: Vec<Box<dyn Fn()>>,
}

impl Defers {
    pub fn defer(&mut self, f: Box<dyn Fn()>) {
        self.stack.push(f);
    }

    pub fn new() -> Defers {
        Defers { stack: Vec::new() }
    }
}

impl Drop for Defers {
    fn drop(&mut self) {
        while let Some(f) = self.stack.pop() {
            f();
        }
    }
}

pub struct Trace {
    pub trace_level: usize,
}

impl Trace {
    pub fn new() -> Trace {
        Trace{ trace_level: 0 }
    }

    fn ident_level(&self) -> String {
        "\t".repeat(&self.trace_level - 1)
    }

    fn incr_ident(&mut self) {
        self.trace_level += 1;
    }

    fn decr_ident(&mut self) {
        self.trace_level -= 1;
    }

    fn trace_print(&self, fs: &str) {
        println!("{}{}", self.ident_level(), fs);
    }

    pub fn trace(&mut self, msg: &str) -> String {
        self.incr_ident();
        self.trace_print(&format!("{}{}", "BEGIN", msg));
        msg.to_string()
    }

    pub fn untrace(&mut self, msg: &str) {
        self.trace_print(&format!("{}{}", "END", msg));
        self.decr_ident();
    }
}