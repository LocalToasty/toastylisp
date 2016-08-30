use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::iter;
use environment::Environment;
use builtin::BuiltinProc;

/// Lisp expressions
#[derive(PartialEq,Debug,Clone)]
pub enum Expr {
    Symbol(String),
    Quote(Rc<Expr>),
    Pair(Rc<Expr>, Rc<Expr>),
    Placeholder,
    If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Cond(Vec<(Rc<Expr>, Rc<Expr>)>),
    Number(i32),
    Boolean(bool),
    Builtin(BuiltinProc),
    Lambda(Vec<String>, Rc<Expr>, Option<Rc<RefCell<Environment>>>),
    Define(String, Rc<Expr>),
    Let(Vec<(String, Rc<Expr>)>, Rc<Expr>),
    Sequence(Vec<Rc<Expr>>),
    Nil,
}

impl Expr {
    // helper functions for easier expression creation
    pub fn new_symbol(symb: &str) -> Rc<Expr> {
        Rc::new(Expr::Symbol(String::from(symb)))
    }

    pub fn new_quote(expr: Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr::Quote(expr))
    }

    pub fn new_pair(head: Rc<Expr>, tail: Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr::Pair(head, tail))
    }

    pub fn new_placeholder() -> Rc<Expr> {
        Rc::new(Expr::Placeholder)
    }

    pub fn new_list(exprs: Vec<Rc<Expr>>) -> Rc<Expr> {
        exprs.iter().rev().fold(Expr::new_nil(),
                                |tail, head| Expr::new_pair(head.clone(), tail.clone()))
    }

    pub fn new_if(pred: Rc<Expr>, cons: Rc<Expr>, alt: Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr::If(pred, cons, alt))
    }

    pub fn new_cond(cases: Vec<(Rc<Expr>, Rc<Expr>)>) -> Rc<Expr> {
        Rc::new(Expr::Cond(cases))
    }

    pub fn new_number(num: i32) -> Rc<Expr> {
        Rc::new(Expr::Number(num))
    }

    /// Creates a new builtin procedure, wrapped in a lambda.
    pub fn new_builtin(procedure: BuiltinProc) -> Rc<Expr> {
        let params: Vec<_> = (0..procedure.param_no()).map(|i| format!("x{}", i)).collect();
        Expr::new_lambda(params.clone(),
                         Expr::new_pair(Rc::new(Expr::Builtin(procedure)),
                                        Expr::new_list(params.iter()
                                                             .map(|name| {
                                                                 Rc::new(Expr::Symbol(name.clone()))
                                                             })
                                                             .collect())),
                         None)
    }

    pub fn new_lambda(args: Vec<String>,
                      body: Rc<Expr>,
                      env: Option<Rc<RefCell<Environment>>>)
                      -> Rc<Expr> {
        Rc::new(Expr::Lambda(args, body, env))
    }

    pub fn new_define(symbol: String, expr: Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr::Define(symbol, expr))
    }

    pub fn new_let(defs: Vec<(String, Rc<Expr>)>, body: Rc<Expr>) -> Rc<Expr> {
        Rc::new(Expr::Let(defs, body))
    }

    pub fn new_sequence(exprs: Vec<Rc<Expr>>) -> Rc<Expr> {
        Rc::new(Expr::Sequence(exprs))
    }

    pub fn new_boolean(val: bool) -> Rc<Expr> {
        Rc::new(Expr::Boolean(val))
    }

    pub fn new_nil() -> Rc<Expr> {
        Rc::new(Expr::Nil)
    }

    pub fn get_type(&self) -> Type {
        match *self {
            Expr::Symbol(_) => Type::Symbol,
            Expr::Quote(_) => Type::Quote,
            Expr::Pair(..) => Type::Pair,
            Expr::Number(_) => Type::Number,
            Expr::Boolean(_) => Type::Boolean,
            Expr::Lambda(..) => Type::Lambda,
            Expr::Nil => Type::Nil,
            _ => Type::Expr,
        }
    }

    fn iter(&self) -> LispListIter {
        LispListIter { list: Rc::new((*self).clone()) }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Symbol(ref name) => write!(f, "{}", name),
            Expr::Quote(ref expr) => write!(f, "'{}", *expr),
            Expr::Pair(ref head, ref tail) => {
                try!(write!(f, "({}", head));
                let mut remainder = tail;
                // as long as the pairs resemble a linked list, render them as such
                while let Expr::Pair(ref head, ref tail) = **remainder {
                    try!(write!(f, " {}", head));
                    remainder = tail;
                }

                // if the last element is a nil, render it as a list, otherwise as a pair
                if let Expr::Nil = **remainder {
                    write!(f, ")")
                } else {
                    write!(f, " . {})", remainder)
                }
            }
            Expr::Placeholder => write!(f, "_"),
            Expr::If(ref pred, ref cons, ref alt) => write!(f, "(if {} {} {})", *pred, *cons, *alt),
            Expr::Cond(ref cases) => {
                try!(write!(f, "(cond"));
                for case in cases {
                    let (ref pred, ref cons) = *case;
                    try!(write!(f, " ({} {})", pred, cons));
                }
                write!(f, ")")
            }
            Expr::Number(val) => write!(f, "{}", val),
            Expr::Boolean(val) => write!(f, "#{}", val),
            Expr::Lambda(ref params, ref body, _) => {
                try!(write!(f, "(lambda ("));
                if !params.is_empty() {
                    try!(write!(f, "{}", params[0]));
                    for param in &params[1..] {
                        try!(write!(f, " {}", param));
                    }
                }
                write!(f, ") {})", body)

            }
            Expr::Builtin(ref procedure) => write!(f, "{}", procedure),
            Expr::Define(ref name, ref expr) => write!(f, "(define {} {})", name, *expr),
            Expr::Let(ref defs, ref body) => {
                try!(write!(f, "(let ("));
                for def in defs {
                    try!(write!(f, "({} {})", def.0, def.1));
                }
                write!(f, ") {})", body)
            }
            Expr::Sequence(ref exprs) => {
                try!(write!(f, "(seqence"));
                for expr in exprs {
                    try!(write!(f, " {}", expr));
                }
                write!(f, ")")
            }
            Expr::Nil => write!(f, "()"),
        }
    }
}

pub struct LispListIter {
    list: Rc<Expr>,
}

impl iter::Iterator for LispListIter {
    type Item = Result<Rc<Expr>, EvalErr>;

    fn next(&mut self) -> Option<Self::Item> {
        match *self.list.clone() {
            Expr::Pair(ref head, ref tail) => {
                let tail = tail.clone();
                self.list = tail;
                Some(Ok(head.clone()))
            }
            Expr::Nil => None,
            _ => {
                Some(Err(EvalErr::TypeErr {
                    expected: Type::Pair,
                    found: self.list.get_type(),
                }))
            }
        }
    }
}


#[derive(PartialEq,Debug,Clone,Copy)]
pub enum Type {
    Symbol,
    Quote,
    Pair,
    Number,
    Boolean,
    Lambda,
    Nil,
    Expr,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Symbol => write!(f, "symbol"),
            Type::Quote => write!(f, "quote"),
            Type::Pair => write!(f, "pair"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Lambda => write!(f, "lambda"),
            Type::Nil => write!(f, "nil"),
            Type::Expr => write!(f, "expression"),
        }
    }
}

#[derive(Debug)]
pub enum EvalErr {
    UndefinedSymbol(String),
    TypeErr {
        expected: Type,
        found: Type,
    },
    TooManyArgs {
        expected: usize,
        found: usize,
    },
    Redefinition(String),
    NonExhaustivePattern,
    LogicError(Vec<Rc<Expr>>),
}

impl fmt::Display for EvalErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EvalErr::UndefinedSymbol(ref symb) => write!(f, "Undefined symbol: {}", symb),
            EvalErr::TypeErr { expected: ex, found: fo } => {
                write!(f, "Type error: Expected {}, found {}", ex, fo)
            }
            EvalErr::TooManyArgs { expected: ex, found: fo } => {
                write!(f, "Too many arguments: Expected {}, found {}", ex, fo)
            }
            EvalErr::Redefinition(ref symb) => write!(f, "Redefinition of symbol '{}'", symb),
            EvalErr::NonExhaustivePattern => write!(f, "Non-exhaustive pattern"),
            EvalErr::LogicError(ref msgs) => {
                try!(write!(f, "Logic error:"));
                for msg in msgs {
                    try!(write!(f, " {}", msg));
                }
                Ok(())
            }
        }
    }
}

pub type EvalRes = Result<Rc<Expr>, EvalErr>;

enum PartialEvalRes {
    /// Evaluation completed.
    Done(Rc<Expr>),
    /// Evaluation is not yet complete, a tail call has to be made.
    TailCall(Rc<Expr>, Vec<Rc<Expr>>),
    /// Evaluation error.
    Err(EvalErr),
}

impl PartialEvalRes {
    fn from_eval_res(res: EvalRes) -> PartialEvalRes {
        match res {
            Ok(expr) => PartialEvalRes::Done(expr),
            Err(err) => PartialEvalRes::Err(err),
        }
    }
}

static mut verbose: Option<usize> = None;

pub fn set_verbose(enabled: bool) {
    if enabled {
        unsafe {
            verbose = Some(0);
        }
    } else {
        unsafe {
            verbose = None;
        }
    }
}

pub fn eval(expr: &Rc<Expr>, env: &Rc<RefCell<Environment>>) -> EvalRes {
    unsafe {
        if let Some(indent) = verbose {
            verbose = Some(indent + 1);
        }
    }
    let mut res = eval_partially(expr, env);

    loop {
        match res {
            PartialEvalRes::TailCall(procedure, args) => res = apply_to_procedure(&procedure, args),
            PartialEvalRes::Done(expr) => {
                unsafe {
                    if let Some(indent) = verbose {
                        verbose = Some(indent - 1);
                        for _ in 0..indent - 1 {
                            print!("   |");
                        }
                        println!("   +-> {}", expr);
                    }
                }
                return Ok(expr);
            }
            PartialEvalRes::Err(err) => return Err(err),
        }
    }
}

/// Evaluates an expression.
///
/// # Errors
///
/// Returns an error if the expression is semantically incorrect.
fn eval_partially(expr: &Rc<Expr>, env: &Rc<RefCell<Environment>>) -> PartialEvalRes {
    unsafe {
        if let Some(indent) = verbose {
            for _ in 0..indent - 1 {
                print!("   |");
            }
            println!("   {}", expr);
        }
    }
    let res = match **expr {
        Expr::Symbol(ref symbol) => {
            // If a symbol is read, return its value.
            // In case of its a lambda, it has to be evaluated again in order to potentially bind
            // its environment.
            match env.borrow().get(symbol) {
                Some(expr) => {
                    match *expr {
                        Expr::Lambda(..) => eval_partially(&expr, env),
                        _ => PartialEvalRes::Done(expr.clone()),
                    }
                }
                _ => PartialEvalRes::Err(EvalErr::UndefinedSymbol(symbol.clone())),
            }
        }
        Expr::Quote(ref e) => PartialEvalRes::Done(e.clone()),
        Expr::Pair(ref head, ref tail) => eval_pair(head, tail, env),
        Expr::If(ref pred, ref cons, ref alt) => eval_if(pred, cons, alt, env),
        Expr::Cond(ref cases) => eval_cond(cases, env),
        Expr::Define(ref symb, ref expr) => {
            PartialEvalRes::from_eval_res(eval_define(symb, expr, env))
        }
        Expr::Let(ref defs, ref body) => eval_let(defs, body, env.clone()),
        Expr::Lambda(ref params, ref body, ref lambda_env) => {
            // If the lambda is not bound to an environment yet, bind it to the current environment.
            // This way variables from this environment are captured.
            let res = match *lambda_env {
                None => Expr::new_lambda(params.clone(), body.clone(), Some(env.clone())),
                _ => expr.clone(),

            };
            PartialEvalRes::Done(res)
        }
        Expr::Sequence(ref exprs) => eval_sequence(exprs, env.clone()),
        // every other expression is self-evaluating
        _ => PartialEvalRes::Done(expr.clone()),
    };

    // if an error occurred, print a the backtrace
    if let PartialEvalRes::Err(_) = res {
        println!("\tin {}", **expr);
    }

    res
}

/// Evaluates a pair.
///
/// Applies the tail of the list to the procedure in its head.  If the length of the tail is less
/// than the number of arguments required by the procedure, a curried version of the procedure is
/// returned.
///
/// # Errors
///
/// Returns EvalErr::TypeErr, if the first element of the list isn't a procedure (i.e. either a
/// lambda or an builtin procedure).  If the tail of the pair does not resemble a list,
/// EvalErr::TypeErr is returned.
fn eval_pair(head: &Rc<Expr>, tail: &Expr, env: &Rc<RefCell<Environment>>) -> PartialEvalRes {
    let first = match eval(head, env) {
        Ok(expr) => expr,
        Err(err) => return PartialEvalRes::Err(err),
    };

    // collect arguments and map them to the parameters
    let args: Vec<_> = match tail.iter().collect() {
        Ok(args) => args,
        Err(err) => return PartialEvalRes::Err(err),
    };
    let mut evaled_args = Vec::new();
    for arg in args {
        match eval(&arg, env) {
            Ok(expr) => evaled_args.push(expr),
            Err(err) => return PartialEvalRes::Err(err),
        };
    }

    match *first {
        Expr::Lambda(ref params, _, _) => {
            // check the number of arguments given
            if evaled_args.len() > params.len() {
                // too many arguments
                PartialEvalRes::Err(EvalErr::TooManyArgs {
                    found: evaled_args.len(),
                    expected: params.len(),
                })
            } else {
                PartialEvalRes::TailCall(first.clone(), evaled_args)
            }
        }
        // built in functions are wrapped in lambdas, thus it is not necassary to do any argument
        // number checking / currying
        Expr::Builtin(ref procedure) => {
            PartialEvalRes::from_eval_res(procedure.eval(evaled_args, env))
        }
        _ => {
            PartialEvalRes::Err(EvalErr::TypeErr {
                expected: Type::Lambda,
                found: first.get_type(),
            })
        }
    }
}

/// Applies arguments to a procedure.
///
/// If the number of arguments matches the adicity of the procedure, and no placeholders were
/// passed, the procedure's body is evaluated with the arguments bound to the corresponding
/// parameters.  Otherwise a new lambda with the remaining parameters having the given arguments
/// bound to it is returned.
///
/// # Panics
///
/// If procedure isn't a lambda.  This should be made impossible by the logic in eval_pair.
fn apply_to_procedure(procedure: &Rc<Expr>, args: Vec<Rc<Expr>>) -> PartialEvalRes {
    let (params, body, lambda_env) = match **procedure {
        Expr::Lambda(ref params, ref body, ref env) => (params, body, env),
        _ => unreachable!(),
    };

    // parameters skipped with place holders
    let mut skipped_params = Vec::new();

    // add arguments to environment
    let lambda_scope = Environment::new_scope(lambda_env.clone().unwrap());
    {
        let mut borrowed_lambda_scope = lambda_scope.borrow_mut();

        let defs = params.iter()
                         .zip(args.iter())
                         .map(|(param, arg)| (param.clone(), arg.clone()));

        for (param, arg) in defs {
            match *arg {
                Expr::Placeholder => skipped_params.push(param),
                _ => borrowed_lambda_scope.insert(param, arg),
            };
        }
    }

    if skipped_params.is_empty() && args.len() == params.len() {
        // correct amount of arguments, evaluate body
        eval_partially(&body, &lambda_scope)
    } else {
        // too few arguments were given; return curried lambda
        let remaining_params = skipped_params.iter()
                                             .chain(params[args.len()..].iter())
                                             .cloned()
                                             .collect();
        PartialEvalRes::Done(Expr::new_lambda(remaining_params, body.clone(), Some(lambda_scope)))
    }
}

/// Evaluates an if expression.
///
/// If pred evaluates to #true, then the result of cons is returned.
/// If it evaluates to #false, alt will be evaluated.
///
/// # Errors
///
/// Returns a TypeErr, if the predicate evaluates to something other than a boolean.
fn eval_if(pred: &Rc<Expr>,
           cons: &Rc<Expr>,
           alt: &Rc<Expr>,
           env: &Rc<RefCell<Environment>>)
           -> PartialEvalRes {
    match eval(pred, env) {
        Ok(expr) => {
            match *expr {
                Expr::Boolean(val) => {
                    if val {
                        eval_partially(cons, env)
                    } else {
                        eval_partially(alt, env)
                    }
                }
                _ => {
                    PartialEvalRes::Err(EvalErr::TypeErr {
                        expected: Type::Boolean,
                        found: expr.get_type(),
                    })
                }
            }
        }
        Err(err) => PartialEvalRes::Err(err),
    }
}

/// Evaluates a cond form.
///
/// # Errors
///
/// If none of the predicates evaluate to #true, EvalErr::NonExhaustivePattern is returned.
/// If one of the predicates does not evaluate to a boolean, TypeErr is returned.
fn eval_cond(cases: &Vec<(Rc<Expr>, Rc<Expr>)>, env: &Rc<RefCell<Environment>>) -> PartialEvalRes {
    for case in cases {
        let (ref pred, ref cons) = *case;

        match eval(pred, env) {
            Ok(expr) => {
                match *expr {
                    Expr::Boolean(val) => {
                        if val {
                            return eval_partially(cons, env);
                        }
                    }
                    _ => {
                        return PartialEvalRes::Err(EvalErr::TypeErr {
                            expected: Type::Boolean,
                            found: expr.get_type(),
                        })
                    }
                }
            }
            Err(err) => return PartialEvalRes::Err(err),
        }
    }

    PartialEvalRes::Err(EvalErr::NonExhaustivePattern)
}

fn eval_define(symb: &String, expr: &Rc<Expr>, env: &Rc<RefCell<Environment>>) -> EvalRes {
    let already_defined = env.borrow().locally_defined(symb);
    if !already_defined {
        let res = try!(eval(expr, &env));
        env.borrow_mut().insert(symb.clone(), res.clone());
        Ok(res)
    } else {
        Err(EvalErr::Redefinition(symb.clone()))
    }
}

/// Evaluates a let expression.
///
/// # Errors
///
/// Returns EvalErr::Redefinition if a symbol is defined twice.
fn eval_let(defs: &Vec<(String, Rc<Expr>)>,
            body: &Rc<Expr>,
            env: Rc<RefCell<Environment>>)
            -> PartialEvalRes {
    let let_env = Environment::new_scope(env);

    // Add definitions to environment
    for def in defs {
        let (ref name, ref expr) = *def;

        // check if the symbol wasn't defined in this scope before
        let already_defined = !let_env.borrow().locally_defined(name);
        if already_defined {
            match eval(&expr, &let_env) {
                Ok(res) => let_env.borrow_mut().insert(name.clone(), res.clone()),
                Err(err) => return PartialEvalRes::Err(err),
            }
        } else {
            return PartialEvalRes::Err(EvalErr::Redefinition(name.clone()));
        }
    }

    match **body {
        Expr::Pair(..) => eval_partially(body, &let_env),
        _ => PartialEvalRes::from_eval_res(eval(body, &let_env)),
    }
}

/// Evaluates a sequence of expressions.
///
/// # Errors
///
/// Returns EvalErr::Redefinition if a symbol is defined twice.
fn eval_sequence(exprs: &Vec<Rc<Expr>>, env: Rc<RefCell<Environment>>) -> PartialEvalRes {
    unsafe {
        if let Some(indent) = verbose {
            verbose = Some(indent + 1);
        }
    }

    let seq_env = Environment::new_scope(env);
    let (last, inits) = exprs.split_last().unwrap();
    for expr in inits {
        if let Err(err) = eval(expr, &seq_env) {
            return PartialEvalRes::Err(err);
        }
    }

    unsafe {
        if let Some(indent) = verbose {
            verbose = Some(indent - 1);
        }
    }

    match **last {
        Expr::Pair(..) => eval_partially(last, &seq_env),
        _ => PartialEvalRes::from_eval_res(eval(last, &seq_env)),
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use std::cell::RefCell;
    use environment::Environment;
    use builtin::BuiltinProc;
    use super::*;

    /// Checks if self-evaluating expressions evaluate to themselves.
    #[test]
    fn eval_self_evaluating() {
        let env = Rc::new(RefCell::new(Environment::new()));

        let number = Expr::new_number(5);
        assert_eq!(number, eval(&number, &env).unwrap());
    }

    #[test]
    fn eval_if() {
        let env = Rc::new(RefCell::new(Environment::new()));
        let res = Expr::new_symbol("expected");

        // predicate not nil
        assert_eq!(res,
                   eval(&Expr::new_if(Expr::new_boolean(true),
                                      Expr::new_quote(res.clone()),
                                      Expr::new_nil()),
                        &env)
                       .unwrap());

        // predicate nil
        assert_eq!(res,
                   eval(&Expr::new_if(Expr::new_boolean(false),
                                      Expr::new_nil(),
                                      Expr::new_quote(res.clone())),
                        &env)
                       .unwrap());
    }

    #[test]
    fn eval_eq() {
        let env = Rc::new(RefCell::new(Environment::new()));
        let x = Expr::new_number(5);
        let y = Expr::new_number(5);

        assert_eq!(Expr::Boolean(true),
                   *eval(&Expr::new_list(vec![Expr::new_builtin(BuiltinProc::Eq), x, y]),
                         &env)
                        .unwrap());
    }
}
