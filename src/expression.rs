use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use environment::Environment;
use builtin::BuiltinProc;

/// Lisp expressions
#[derive(PartialEq,Debug)]
pub enum Expr {
    Symbol(String),
    Quote(Rc<Expr>),
    Pair(Rc<Expr>, Rc<Expr>),
    If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    Cond(Vec<(Rc<Expr>, Rc<Expr>)>),
    Number(i32),
    Builtin(BuiltinProc),
    Lambda(Vec<String>, Rc<Expr>, Option<Rc<RefCell<Environment>>>),
    Define(String, Rc<Expr>),
    Let(Vec<(String, Rc<Expr>)>, Rc<Expr>),
    Sequence(Vec<Rc<Expr>>),
    True,
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

    /// Creates a new intrinsic procedure, wrapped in a lambda.
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

    pub fn new_true() -> Rc<Expr> {
        Rc::new(Expr::True)
    }

    pub fn new_nil() -> Rc<Expr> {
        Rc::new(Expr::Nil)
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
            Expr::True => write!(f, "true"),
            Expr::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum EvalErr {
    UndefinedSymbol(String),
    TypeErr,
    ArgListExpected,
    ProcExpected,
    TooManyArgs {
        expected: usize,
        found: usize,
    },
    Redefinition(String),
    NonExhaustivePattern,
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

pub fn eval(expr: &Rc<Expr>, env: &Rc<Environment>) -> EvalRes {
    let mut res = eval_partially(expr, env);

    loop {
        match res {
            PartialEvalRes::TailCall(procedure, args) => res = apply_procedure(&procedure, args),
            PartialEvalRes::Done(expr) => return Ok(expr),
            PartialEvalRes::Err(err) => return Err(err),
        }
    }
}

/// Evaluates an expression.
///
/// # Errors
///
/// Returns an error if the expression is semantically incorrect.
fn eval_partially(expr: &Rc<Expr>, env: &Rc<Environment>) -> PartialEvalRes {
    let res = match **expr {
        Expr::Symbol(ref symbol) => {
            // If a symbol is read, return its value.
            // In case of its a lambda, it has to be evaluated again in order to potentially bind
            // its environment.
            match env.get(symbol) {
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
        Expr::Define(_, ref expr) => eval_partially(expr, env),
        Expr::Let(ref defs, ref body) => eval_let(defs, body, env.clone()),
        Expr::Lambda(ref params, ref body, ref lambda_env) => {
            // If the lambda is not bound to an environment yet, bind it to the current environment.
            // This way variables from this environment are captured.
            let res = match *lambda_env {
                None => {
                    let wrapped_env = Rc::new(RefCell::new((**env).clone()));
                    Expr::new_lambda(params.clone(), body.clone(), Some(wrapped_env))
                }
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
/// Returns EvalErr::ProcExpected, if the first element of the list isn't a procedure (i.e. either a
/// lambda or an intrinsic procedure).  If the tail of the pair does not resemble a list,
/// EvalErr::ArgListExpected is returned.
fn eval_pair(head: &Rc<Expr>, tail: &Expr, env: &Rc<Environment>) -> PartialEvalRes {
    let first = match eval(head, env) {
        Ok(expr) => expr,
        Err(err) => return PartialEvalRes::Err(err),
    };

    match *first {
        Expr::Lambda(ref params, _, _) => {
            // collect arguments and map them to the parameters
            let args = match collect_list(tail) {
                Ok(args) => args,
                Err(err) => return PartialEvalRes::Err(err),
            };

            // check the number of arguments given
            if args.len() > params.len() {
                // too many arguments
                PartialEvalRes::Err(EvalErr::TooManyArgs {
                    found: args.len(),
                    expected: params.len(),
                })
            } else {
                let mut evaled_args = Vec::new();
                for arg in args {
                    match eval(&arg, env) {
                        Ok(expr) => evaled_args.push(expr),
                        Err(err) => return PartialEvalRes::Err(err),
                    };
                }
                PartialEvalRes::TailCall(first.clone(), evaled_args)
            }
        }
        // built in functions are wrapped in lambdas, thus it is not necassary to do any argument
        // number checking / currying
        Expr::Builtin(ref procedure) => {
            // collect arguments and map them to the parameters
            match collect_list(tail) {
                Ok(args) => PartialEvalRes::from_eval_res(procedure.apply(&args, env)),
                Err(err) => PartialEvalRes::Err(err),
            }
        }
        _ => PartialEvalRes::Err(EvalErr::ProcExpected),
    }
}

fn apply_procedure(procedure: &Rc<Expr>, args: Vec<Rc<Expr>>) -> PartialEvalRes {
    let (params, body, lambda_env) = match **procedure {
        Expr::Lambda(ref params, ref body, ref env) => (params, body, env),
        _ => unreachable!(),
    };

    // add arguments to environment
    let unwrapped_lambda_env = (*lambda_env.clone().unwrap()).clone().into_inner();
    let mut lambda_scope = Environment::new_scope(Rc::new(unwrapped_lambda_env));

    let defs = params.iter()
                     .zip(args.iter())
                     .map(|(param, arg)| (param.clone(), arg.clone()));
    for (param, arg) in defs {
        lambda_scope.insert(param, arg);
    }

    if args.len() == params.len() {
        // correct amount of arguments, evaluate body
        eval_partially(&body, &Rc::new(lambda_scope))
    } else {
        // too few arguments were given; return curried lambda
        let remaining_params = params[args.len()..].to_vec();
        PartialEvalRes::Done(Expr::new_lambda(remaining_params,
                                              body.clone(),
                                              Some(Rc::new(RefCell::new(lambda_scope)))))
    }
}

/// Transforms a Lisp list into a vector.
///
/// # Errors
///
/// Returns an EvalErr::ArgListExpected, if list is not a list.
fn collect_list(mut list: &Expr) -> Result<Vec<Rc<Expr>>, EvalErr> {
    let mut res = Vec::new();

    loop {
        match *list {
            Expr::Nil => return Ok(res),
            Expr::Pair(ref head, ref tail) => {
                res.push(head.clone());
                list = tail;
            }
            _ => return Err(EvalErr::ArgListExpected),
        }
    }
}

/// Evaluates an if expression.
///
/// If pred evaluates to something other than Expr::Nil, then the result of cons is returned.
/// Otherwise alt will be evaluated.
fn eval_if(pred: &Rc<Expr>,
           cons: &Rc<Expr>,
           alt: &Rc<Expr>,
           env: &Rc<Environment>)
           -> PartialEvalRes {
    match eval(pred, env) {
        Ok(expr) => {
            if *expr != Expr::Nil {
                eval_partially(cons, env)
            } else {
                eval_partially(alt, env)
            }
        }
        Err(err) => PartialEvalRes::Err(err),
    }
}

/// Evaluates a cond form.
///
/// # Errors
///
/// If none of the predicates evaluate to a value different from nil, EvalErr::NonExhaustivePattern
/// is returned.
fn eval_cond(cases: &Vec<(Rc<Expr>, Rc<Expr>)>, env: &Rc<Environment>) -> PartialEvalRes {
    for case in cases {
        let (ref pred, ref cons) = *case;

        match eval(pred, env) {
            Ok(expr) => {
                if *expr != Expr::Nil {
                    return eval_partially(&cons, env);
                }
            }
            Err(err) => return PartialEvalRes::Err(err),
        }
    }

    PartialEvalRes::Err(EvalErr::NonExhaustivePattern)
}

/// Evaluates a let expression.
///
/// # Errors
///
/// Returns EvalErr::Redefinition if a symbol is defined twice.
fn eval_let(defs: &Vec<(String, Rc<Expr>)>,
            body: &Rc<Expr>,
            env: Rc<Environment>)
            -> PartialEvalRes {
    let mut let_env = Environment::new_scope(env);

    // Add definitions to environment
    for def in defs {
        let (ref name, ref expr) = *def;

        // check if the symbol wasn't defined in this scope before
        if !let_env.locally_defined(&name) {
            match eval(&expr, &Rc::new(let_env.clone())) {
                Ok(res) => {
                    if let Expr::Lambda(_, _, ref env) = *res {
                        // capture lambda args
                        env.as_ref().unwrap().borrow_mut().insert(name.clone(), res.clone());
                    }
                    let_env.insert(name.clone(), res.clone());
                }
                Err(err) => return PartialEvalRes::Err(err),
            }
        } else {
            return PartialEvalRes::Err(EvalErr::Redefinition(name.clone()));
        }
    }

    match **body {
        Expr::Pair(..) => eval_partially(body, &Rc::new(let_env)),
        _ => PartialEvalRes::from_eval_res(eval(body, &Rc::new(let_env))),
    }
}

/// Evaluates a sequence of expressions.
///
/// # Errors
///
/// Returns EvalErr::Redefinition if a symbol is defined twice.
fn eval_sequence(exprs: &Vec<Rc<Expr>>, env: Rc<Environment>) -> PartialEvalRes {
    let mut seq_env = Environment::new_scope(env);
    let (last, inits) = exprs.split_last().unwrap();
    for expr in inits {
        match **expr {
            Expr::Define(ref name, ref expr) => {
                if !seq_env.locally_defined(name) {
                    match eval(expr, &Rc::new(seq_env.clone())) {
                        Ok(res) => {
                            if let Expr::Lambda(_, _, ref env) = *res {
                                env.as_ref()
                                   .unwrap()
                                   .borrow_mut()
                                   .insert(name.clone(), res.clone());
                            }
                            seq_env.insert(name.clone(), res.clone());
                        }
                        Err(err) => return PartialEvalRes::Err(err),
                    }
                } else {
                    return PartialEvalRes::Err(EvalErr::Redefinition(name.clone()));
                }
            }
            _ => {
                if let Err(err) = eval(expr, &Rc::new(seq_env.clone())) {
                    return PartialEvalRes::Err(err);
                }
            }
        }
    }

    match **last {
        Expr::Pair(..) => eval_partially(last, &Rc::new(seq_env)),
        _ => PartialEvalRes::from_eval_res(eval(last, &Rc::new(seq_env))),
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use environment::Environment;
    use builtin::BuiltinProc;
    use super::*;

    /// Checks if self-evaluating expressions evaluate to themselves.
    #[test]
    fn eval_self_evaluating() {
        let env = Rc::new(Environment::new());

        let number = Expr::new_number(5);
        assert_eq!(number, eval(&number, &env).unwrap());
    }

    #[test]
    fn eval_if() {
        let env = Rc::new(Environment::new());
        let res = Expr::new_symbol("expected");

        // predicate not nil
        assert_eq!(res,
                   eval(&Expr::new_if(Expr::new_number(0),
                                      Expr::new_quote(res.clone()),
                                      Expr::new_nil()),
                        &env)
                       .unwrap());

        // predicate nil
        assert_eq!(res,
                   eval(&Expr::new_if(Expr::new_nil(),
                                      Expr::new_nil(),
                                      Expr::new_quote(res.clone())),
                        &env)
                       .unwrap());
    }

    #[test]
    fn eval_eq() {
        let env = Rc::new(Environment::new());
        let x = Expr::new_number(5);
        let y = Expr::new_number(5);

        assert_eq!(Expr::True,
                   *eval(&Expr::new_list(vec![Expr::new_builtin(BuiltinProc::Eq), x, y]),
                         &env)
                        .unwrap());
    }
}
