use std::rc::Rc;
use std::fmt;
use expression::{Expr, eval, EvalRes, EvalErr};
use environment::Environment;

/// Intrinsic procedures.
#[derive(PartialEq,Debug)]
pub enum IntrinsicProc {
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

impl IntrinsicProc {
    /// Returns the number of arguments taken by the intrinsic procedure.
    pub fn param_no(&self) -> usize {
        match *self {
            IntrinsicProc::Add |
            IntrinsicProc::Sub |
            IntrinsicProc::Mul |
            IntrinsicProc::Div |
            IntrinsicProc::Mod |
            IntrinsicProc::Eq |
            IntrinsicProc::Lt |
            IntrinsicProc::Gt |
            IntrinsicProc::And |
            IntrinsicProc::Or |
            IntrinsicProc::Cons => 2,
            IntrinsicProc::Not |
            IntrinsicProc::Head |
            IntrinsicProc::Tail |
            IntrinsicProc::IsDefined |
            IntrinsicProc::IsNumber |
            IntrinsicProc::IsQuote |
            IntrinsicProc::IsLambda |
            IntrinsicProc::IsPair |
            IntrinsicProc::IsNil |
            IntrinsicProc::Print => 1,
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
            IntrinsicProc::Add => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i + j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            IntrinsicProc::Sub => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i - j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            IntrinsicProc::Mul => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i * j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            IntrinsicProc::Div => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i / j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            IntrinsicProc::Mod => {
                if let (&Expr::Number(i), &Expr::Number(j)) = (&*(args[0]), &*(args[1])) {
                    Ok(Expr::new_number(i % j))
                } else {
                    Err(EvalErr::TypeErr)
                }
            }
            IntrinsicProc::Eq => {
                if args[0] == args[1] {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            IntrinsicProc::Gt => {
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
            IntrinsicProc::Lt => {
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
            IntrinsicProc::And => {
                if *args[0] != Expr::Nil && *args[1] != Expr::Nil {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            IntrinsicProc::Or => {
                if *args[0] != Expr::Nil || *args[1] != Expr::Nil {
                    Ok(Expr::new_true())
                } else {
                    Ok(Expr::new_nil())
                }
            }
            IntrinsicProc::Not => {
                match *args[0] {
                    Expr::Nil => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::Cons => Ok(Expr::new_pair(args[0].clone(), args[1].clone())),
            IntrinsicProc::Head => {
                match *args[0] {
                    Expr::Pair(ref head, _) => Ok(head.clone()),
                    _ => Err(EvalErr::TypeErr),
                }
            }
            IntrinsicProc::Tail => {
                match *args[0] {
                    Expr::Pair(_, ref tail) => Ok(tail.clone()),
                    _ => Err(EvalErr::TypeErr),
                }
            }
            IntrinsicProc::IsDefined => {
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
            IntrinsicProc::IsQuote => {
                match *args[0] {
                    Expr::Quote(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::IsNumber => {
                match *args[0] {
                    Expr::Number(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::IsLambda => {
                match *args[0] {
                    Expr::Number(_) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::IsPair => {
                match *args[0] {
                    Expr::Pair(..) => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::IsNil => {
                match *args[0] {
                    Expr::Nil => Ok(Expr::new_true()),
                    _ => Ok(Expr::new_nil()),
                }
            }
            IntrinsicProc::Print => {
                println!("{}", args[0]);
                Ok(Expr::new_nil())
            }
        }
    }
}

impl fmt::Display for IntrinsicProc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IntrinsicProc::Add => write!(f, "intrinsic.+"),
            IntrinsicProc::Sub => write!(f, "intrinsic.-"),
            IntrinsicProc::Mul => write!(f, "intrinsic.*"),
            IntrinsicProc::Div => write!(f, "intrinsic./"),
            IntrinsicProc::Mod => write!(f, "intrinsic.mod"),
            IntrinsicProc::Eq => write!(f, "intrinsic.="),
            IntrinsicProc::Lt => write!(f, "intrinsic.<"),
            IntrinsicProc::Gt => write!(f, "intrinsic.>"),
            IntrinsicProc::And => write!(f, "intrinsic.and"),
            IntrinsicProc::Or => write!(f, "intrinsic.or"),
            IntrinsicProc::Not => write!(f, "intrinsic.not"),
            IntrinsicProc::Cons => write!(f, "intrinsic.cons"),
            IntrinsicProc::Head => write!(f, "intrinsic.head"),
            IntrinsicProc::Tail => write!(f, "intrinsic.tail"),
            IntrinsicProc::IsDefined => write!(f, "intrinsic.defined?"),
            IntrinsicProc::IsQuote => write!(f, "intrinsic.quote?"),
            IntrinsicProc::IsNumber => write!(f, "intrinsic.number?"),
            IntrinsicProc::IsLambda => write!(f, "intrinsic.lambda?"),
            IntrinsicProc::IsPair => write!(f, "intrinsic.pair?"),
            IntrinsicProc::IsNil => write!(f, "intrinsic.nil?"),
            IntrinsicProc::Print => write!(f, "intrinsic.print!"),
        }
    }
}
