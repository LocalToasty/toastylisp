use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use expression::Expr;
use builtin::BuiltinProc;

#[derive(Clone,PartialEq,Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
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
        env.insert(String::from("+"), Expr::new_builtin(BuiltinProc::Add));
        env.insert(String::from("-"), Expr::new_builtin(BuiltinProc::Sub));
        env.insert(String::from("*"), Expr::new_builtin(BuiltinProc::Mul));
        env.insert(String::from("/"), Expr::new_builtin(BuiltinProc::Div));
        env.insert(String::from("mod"), Expr::new_builtin(BuiltinProc::Mod));
        env.insert(String::from("="), Expr::new_builtin(BuiltinProc::Eq));
        env.insert(String::from("<"), Expr::new_builtin(BuiltinProc::Lt));
        env.insert(String::from(">"), Expr::new_builtin(BuiltinProc::Gt));
        env.insert(String::from("and"), Expr::new_builtin(BuiltinProc::And));
        env.insert(String::from("or"), Expr::new_builtin(BuiltinProc::Or));
        env.insert(String::from("not"), Expr::new_builtin(BuiltinProc::Not));
        env.insert(String::from("cons"), Expr::new_builtin(BuiltinProc::Cons));
        env.insert(String::from("head"), Expr::new_builtin(BuiltinProc::Head));
        env.insert(String::from("tail"), Expr::new_builtin(BuiltinProc::Tail));
        env.insert(String::from("defined?"),
                   Expr::new_builtin(BuiltinProc::IsDefined));
        env.insert(String::from("number?"),
                   Expr::new_builtin(BuiltinProc::IsNumber));
        env.insert(String::from("boolean?"),
                   Expr::new_builtin(BuiltinProc::IsBoolean));
        env.insert(String::from("quote?"),
                   Expr::new_builtin(BuiltinProc::IsQuote));
        env.insert(String::from("lambda?"),
                   Expr::new_builtin(BuiltinProc::IsLambda));
        env.insert(String::from("pair?"),
                   Expr::new_builtin(BuiltinProc::IsPair));
        env.insert(String::from("nil?"), Expr::new_builtin(BuiltinProc::IsNil));
        env.insert(String::from("print!"),
                   Expr::new_builtin(BuiltinProc::Print));

        env
    }

    pub fn new_scope(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            parent: Some(parent),
            local: HashMap::new(),
        }))
    }

    pub fn get(&self, symbol: &String) -> Option<Rc<Expr>> {
        if let Some(val) = self.local.get(symbol) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(symbol)
        } else {
            None
        }
    }

    pub fn is_defined(&self, symbol: &String) -> bool {
        self.local.contains_key(symbol) ||
        self.parent.as_ref().map_or(false, |p| p.borrow().is_defined(symbol))
    }

    pub fn locally_defined(&self, symbol: &String) -> bool {
        self.local.contains_key(symbol)
    }

    pub fn insert(&mut self, symbol: String, value: Rc<Expr>) {
        self.local.insert(symbol, value);
    }
}
