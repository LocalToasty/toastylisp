use std::rc::Rc;
use std::collections::HashMap;
use expression::Expr;
use intrinsic::IntrinsicProc;

#[derive(Clone,PartialEq,Debug)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    local: HashMap<String, Rc<Expr>>,
}

impl Environment {
    /// Creates a new environment.
    pub fn new() -> Environment {
        let mut env = Environment {
            parent: None,
            local: HashMap::new(),
        };

        // define intrinsic functions
        env.insert(String::from("+"), Expr::new_intrinsic(IntrinsicProc::Add));
        env.insert(String::from("-"), Expr::new_intrinsic(IntrinsicProc::Sub));
        env.insert(String::from("*"), Expr::new_intrinsic(IntrinsicProc::Mul));
        env.insert(String::from("/"), Expr::new_intrinsic(IntrinsicProc::Div));
        env.insert(String::from("mod"), Expr::new_intrinsic(IntrinsicProc::Mod));
        env.insert(String::from("="), Expr::new_intrinsic(IntrinsicProc::Eq));
        env.insert(String::from("<"), Expr::new_intrinsic(IntrinsicProc::Lt));
        env.insert(String::from(">"), Expr::new_intrinsic(IntrinsicProc::Gt));
        env.insert(String::from("and"), Expr::new_intrinsic(IntrinsicProc::And));
        env.insert(String::from("or"), Expr::new_intrinsic(IntrinsicProc::Or));
        env.insert(String::from("not"), Expr::new_intrinsic(IntrinsicProc::Not));
        env.insert(String::from("cons"),
                   Expr::new_intrinsic(IntrinsicProc::Cons));
        env.insert(String::from("head"),
                   Expr::new_intrinsic(IntrinsicProc::Head));
        env.insert(String::from("tail"),
                   Expr::new_intrinsic(IntrinsicProc::Tail));
        env.insert(String::from("defined?"),
                   Expr::new_intrinsic(IntrinsicProc::IsDefined));
        env.insert(String::from("number?"),
                   Expr::new_intrinsic(IntrinsicProc::IsNumber));
        env.insert(String::from("quote?"),
                   Expr::new_intrinsic(IntrinsicProc::IsQuote));
        env.insert(String::from("lambda?"),
                   Expr::new_intrinsic(IntrinsicProc::IsLambda));
        env.insert(String::from("pair?"),
                   Expr::new_intrinsic(IntrinsicProc::IsPair));
        env.insert(String::from("nil?"),
                   Expr::new_intrinsic(IntrinsicProc::IsNil));
        env.insert(String::from("print!"),
                   Expr::new_intrinsic(IntrinsicProc::Print));

        env.insert(String::from("true"), Expr::new_true());
        env.insert(String::from("nil"), Expr::new_nil());

        env
    }

    pub fn new_scope(parent: Rc<Environment>) -> Environment {
        Environment {
            parent: Some(parent),
            local: HashMap::new(),
        }
    }

    pub fn get(&self, symbol: &String) -> Option<Rc<Expr>> {
        if let Some(val) = self.local.get(symbol) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(symbol)
        } else {
            None
        }
    }

    pub fn is_defined(&self, symbol: &String) -> bool {
        if self.local.contains_key(symbol) {
            true
        } else if let Some(ref parent) = self.parent {
            parent.is_defined(symbol)
        } else {
            false
        }
    }

    pub fn locally_defined(&self, symbol: &String) -> bool {
        self.local.contains_key(symbol)
    }

    pub fn insert(&mut self, symbol: String, value: Rc<Expr>) {
        self.local.insert(symbol, value);
    }
}
