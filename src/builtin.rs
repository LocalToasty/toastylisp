use std::rc::Rc;
use std::fmt;
use expression::{Expr, eval, EvalRes, EvalErr};
use environment::Environment;

/// Intrinsic procedures.
#[derive(PartialEq,Debug)]
pub enum BuiltinProc {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Integer division
    Div,
    /// Modulo
    Mod,
    /// Test for equality
    Eq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Logical conjunction
    And,
    /// Logical disjunction
    Or,
    /// Logical negation
    Not,
    /// Pair constructor
    Cons,
    /// Get first element of pair
    Head,
    /// Get second element of pair
    Tail,
    /// Check if expression evaluates to a defined symbol.
    IsDefined,
    /// Check if expression evaluates to a number.
    IsNumber,
    /// Check if expression evaluates to a quotation.
    IsQuote,
    /// Check if expression evaluates to a lambda.
    IsLambda,
    /// Check if expression evaluates to a pair.
    IsPair,
    /// Check if expression evaluates to nil.
    IsNil,
    /// Print an expression.
    Print,
}

impl BuiltinProc {
    /// Returns the number of arguments taken by the intrinsic procedure.
    pub fn param_no(&self) -> usize {
        match *self {
            BuiltinProc::Add |
            BuiltinProc::Sub |
            BuiltinProc::Mul |
            BuiltinProc::Div |
            BuiltinProc::Mod |
            BuiltinProc::Eq |
            BuiltinProc::Lt |
            BuiltinProc::Gt |
            BuiltinProc::And |
            BuiltinProc::Or |
            BuiltinProc::Cons => 2,
            BuiltinProc::Not |
            BuiltinProc::Head |
            BuiltinProc::Tail |
            BuiltinProc::IsDefined |
            BuiltinProc::IsNumber |
            BuiltinProc::IsQuote |
            BuiltinProc::IsLambda |
            BuiltinProc::IsPair |
            BuiltinProc::IsNil |
            BuiltinProc::Print => 1,
        }
    }

    /// Applies the given arguments to the intrinsic procedure.
    ///
    /// # Errors
    ///
    /// If any of the arguments has the wrong type, EvalErr::TypeErr is returned.
    ///
    /// # Panics
    ///
    /// The function panics if too few arguments are supplied.
    pub fn apply(&self, args: &Vec<Rc<Expr>>, env: &Rc<Environment>) -> EvalRes {
        let args: Vec<_> = args.iter().map(|expr| eval(expr, env).unwrap()).collect();
        match *self {
            BuiltinProc::Add => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i + j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Sub => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i - j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Mul => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i * j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Div => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i / j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Mod => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i % j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Eq => {
                if args[0] == args[1] {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            BuiltinProc::Gt => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    if i > j {
                        Ok(Expr::new_true())
                    } else {
                        Ok(Expr::new_nil())
                    }
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::Lt => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    if i < j {
                        Ok(Expr::new_true())
                    } else {
                        Ok(Expr::new_nil())
                    }
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::And => {
                if *args[0] != Expr::Nil && *args[1] != Expr::Nil {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            BuiltinProc::Or => {
                if *args[0] != Expr::Nil || *args[1] != Expr::Nil {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            BuiltinProc::Not => {
                match *args[0] {
                    Expr::Nil => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::Cons => Ok(Expr::new_pair(args[0].clone(), args[1].clone())),
            BuiltinProc::Head => {
                match *args[0] {
                    Expr::Pair(ref head, _) => Ok(head.clone()),
                    _ => Err(EvalErr::TypeErr),
                }
            }
            BuiltinProc::Tail => {
                match *args[0] {
                    Expr::Pair(_, ref tail) => Ok(tail.clone()),
                    _ => Err(EvalErr::TypeErr),
                }
            }
            BuiltinProc::IsDefined => {
                if let Expr::Symbol(ref name) = *args[0] {
                    if env.is_defined(name) {
                        Ok(Expr::new_true())
                    } else {
                        Ok(Expr::new_nil())
                    }
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            BuiltinProc::IsQuote => {
                match *args[0] {
                    Expr::Quote(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::IsNumber => {
                match *args[0] {
                    Expr::Number(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::IsLambda => {
                match *args[0] {
                    Expr::Number(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::IsPair => {
                match *args[0] {
                    Expr::Pair(..) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::IsNil => {
                match *args[0] {
                    Expr::Nil => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            BuiltinProc::Print => {
                println!("{}", args[0]);
                Ok(Expr::new_nil())
            }
        }
    }
}

impl fmt::Display for BuiltinProc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BuiltinProc::Add => write!(f, "builtin.+"),
            BuiltinProc::Sub => write!(f, "builtin.-"),
            BuiltinProc::Mul => write!(f, "builtin.*"),
            BuiltinProc::Div => write!(f, "builtin./"),
            BuiltinProc::Mod => write!(f, "builtin.mod"),
            BuiltinProc::Eq => write!(f, "builtin.="),
            BuiltinProc::Lt => write!(f, "builtin.<"),
            BuiltinProc::Gt => write!(f, "builtin.>"),
            BuiltinProc::And => write!(f, "builtin.and"),
            BuiltinProc::Or => write!(f, "builtin.or"),
            BuiltinProc::Not => write!(f, "builtin.not"),
            BuiltinProc::Cons => write!(f, "builtin.cons"),
            BuiltinProc::Head => write!(f, "builtin.head"),
            BuiltinProc::Tail => write!(f, "builtin.tail"),
            BuiltinProc::IsDefined => write!(f, "builtin.defined?"),
            BuiltinProc::IsQuote => write!(f, "builtin.quote?"),
            BuiltinProc::IsNumber => write!(f, "builtin.number?"),
            BuiltinProc::IsLambda => write!(f, "builtin.lambda?"),
            BuiltinProc::IsPair => write!(f, "builtin.pair?"),
            BuiltinProc::IsNil => write!(f, "builtin.nil?"),
            BuiltinProc::Print => write!(f, "builtin.print!"),
        }
    }
}
