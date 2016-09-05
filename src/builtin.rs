use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use expression::{Expr, Type, eval, EvalRes, EvalErr};
use environment::Environment;

/// Intrinsic procedures.
#[derive(PartialEq,Debug,Clone)]
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
    /// Evaluate a quoted expression.
    Eval,
    /// Check if expression evaluates to a defined symbol.
    IsDefined,
    /// Check if expression evaluates to a number.
    IsNumber,
    /// Check if expression evaluates to a boolean.
    IsBoolean,
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
    /// Terminates the program with an error message.
    Error,
}

impl BuiltinProc {
    /// Returns the number of arguments taken by the intrinsic procedure, as well as if it's
    /// variadic or not.
    pub fn param_no(&self) -> (bool, usize) {
        match *self {
            BuiltinProc::Add |
            BuiltinProc::Mul |
            BuiltinProc::And |
            BuiltinProc::Or => (true, 1),
            BuiltinProc::Eq |
            BuiltinProc::Lt |
            BuiltinProc::Gt => (true, 3),
            BuiltinProc::Sub |
            BuiltinProc::Div |
            BuiltinProc::Mod |
            BuiltinProc::Cons => (false, 2),
            BuiltinProc::Not |
            BuiltinProc::Head |
            BuiltinProc::Tail |
            BuiltinProc::Eval |
            BuiltinProc::IsDefined |
            BuiltinProc::IsNumber |
            BuiltinProc::IsBoolean |
            BuiltinProc::IsQuote |
            BuiltinProc::IsLambda |
            BuiltinProc::IsPair |
            BuiltinProc::IsNil => (false, 1),
            BuiltinProc::Print |
            BuiltinProc::Error => (true, 1),
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
    pub fn eval(&self, args: Vec<Rc<Expr>>, env: &Rc<RefCell<Environment>>) -> EvalRes {
        match *self {
            BuiltinProc::Add |
            BuiltinProc::Mul => self.eval_var_numeric(args),
            BuiltinProc::Sub |
            BuiltinProc::Div |
            BuiltinProc::Mod => self.eval_fixed_numeric(args),
            BuiltinProc::Lt |
            BuiltinProc::Gt => self.eval_ord(args),
            BuiltinProc::Eq => BuiltinProc::eval_equal(args),
            BuiltinProc::And | BuiltinProc::Or => self.eval_logic_junctor(args),
            BuiltinProc::Not => {
                match *args[0] {
                    Expr::Boolean(val) => Ok(Expr::new_boolean(!val)),
                    ref res @ _ => {
                        Err(EvalErr::TypeErr {
                            expected: Type::Boolean,
                            found: res.get_type(),
                        })
                    }
                }
            }
            BuiltinProc::Cons => BuiltinProc::eval_cons(args),
            BuiltinProc::Head | BuiltinProc::Tail => self.eval_head_tail(args),
            BuiltinProc::Eval => eval(&args[0], env),
            BuiltinProc::IsDefined => {
                if let Expr::Symbol(ref name) = *args[0] {
                    if env.borrow().is_defined(name) {
                        Ok(Expr::new_boolean(true))
                    } else {
                        Ok(Expr::new_boolean(false))
                    }
                } else {
                    Err(EvalErr::TypeErr {
                        expected: Type::Symbol,
                        found: args[0].get_type(),
                    })
                }
            }
            BuiltinProc::IsQuote => {
                match *args[0] {
                    Expr::Quote(_) => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::IsNumber => {
                match *args[0] {
                    Expr::Number(_) => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::IsBoolean => {
                match *args[0] {
                    Expr::Boolean(_) => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::IsLambda => {
                match *args[0] {
                    Expr::Lambda{..} => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::IsPair => {
                match *args[0] {
                    Expr::Pair(..) => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::IsNil => {
                match *args[0] {
                    Expr::Nil => Ok(Expr::new_boolean(true)),
                    _ => Ok(Expr::new_boolean(false)),
                }
            }
            BuiltinProc::Print => BuiltinProc::eval_print(args),
            BuiltinProc::Error => BuiltinProc::eval_error(args),
        }
    }

    fn eval_var_numeric(&self, args: Vec<Rc<Expr>>) -> EvalRes {
        let mut num_args = Vec::new();
        for arg in args[0].iter() {
            match *try!(arg) {
                Expr::Number(n) => num_args.push(n),
                ref a @ _ => {
                    return Err(EvalErr::TypeErr {
                        expected: Type::Number,
                        found: a.get_type(),
                    })
                }
            }
        }

        let iter = num_args.iter();

        match *self {
            BuiltinProc::Add => Ok(Expr::new_number(iter.fold(0, |acc, x| acc + x))),
            BuiltinProc::Mul => Ok(Expr::new_number(iter.fold(1, |acc, x| acc * x))),
            _ => unreachable!(),
        }
    }

    fn eval_fixed_numeric(&self, args: Vec<Rc<Expr>>) -> EvalRes {
        let mut num_args = Vec::new();
        for arg in args {
            match *arg {
                Expr::Number(n) => num_args.push(n),
                ref a @ _ => {
                    return Err(EvalErr::TypeErr {
                        expected: Type::Number,
                        found: a.get_type(),
                    })
                }
            }
        }

        match *self {
            BuiltinProc::Sub => Ok(Expr::new_number(num_args[0] - num_args[1])),
            BuiltinProc::Div => Ok(Expr::new_number(num_args[0] / num_args[1])),
            BuiltinProc::Mod => Ok(Expr::new_number(num_args[0] % num_args[1])),
            _ => unreachable!(),
        }
    }

    fn eval_equal(args: Vec<Rc<Expr>>) -> EvalRes {
        if args[0] != args[1] {
            return Ok(Expr::new_boolean(false));
        }

        let mut last = args[1].clone();
        for arg in args[2].iter() {
            let curr = try!(arg);

            if last != curr {
                return Ok(Expr::new_boolean(false));
            }

            last = curr;
        }
        Ok(Expr::new_boolean(true))
    }

    fn eval_ord(&self, args: Vec<Rc<Expr>>) -> EvalRes {
        let mut last = if let Expr::Number(n) = *args[0] {
            n
        } else {
            return Err(EvalErr::TypeErr {
                expected: Type::Number,
                found: args[0].get_type(),
            });
        };

        let mut num_args = Vec::new();

        if let Expr::Number(n) = *args[1] {
            num_args.push(n);
        } else {
            return Err(EvalErr::TypeErr {
                expected: Type::Number,
                found: args[1].get_type(),
            });
        };
        for arg in args[2].iter() {
            match *try!(arg) {
                Expr::Number(n) => num_args.push(n),
                ref a @ _ => {
                    return Err(EvalErr::TypeErr {
                        expected: Type::Number,
                        found: a.get_type(),
                    })
                }
            }
        }

        match *self {
            BuiltinProc::Lt => {
                for arg in num_args {
                    if !(last < arg) {
                        return Ok(Expr::new_boolean(false));
                    }

                    last = arg;
                }
                Ok(Expr::new_boolean(true))
            }
            BuiltinProc::Gt => {
                for arg in num_args {
                    if !(last > arg) {
                        return Ok(Expr::new_boolean(false));
                    }

                    last = arg;
                }
                Ok(Expr::new_boolean(true))
            }
            _ => unreachable!(),
        }
    }

    fn eval_logic_junctor(&self, args: Vec<Rc<Expr>>) -> EvalRes {
        let mut boolean_args = Vec::new();
        for arg in args[0].iter() {
            match *try!(arg) {
                Expr::Boolean(b) => boolean_args.push(b),
                ref a @ _ => {
                    return Err(EvalErr::TypeErr {
                        expected: Type::Boolean,
                        found: a.get_type(),
                    })
                }
            }
        }

        match *self {
            BuiltinProc::And => {
                Ok(Expr::new_boolean(boolean_args.iter().fold(true, |acc, &x| acc && x)))
            }
            BuiltinProc::Or => {
                Ok(Expr::new_boolean(boolean_args.iter().fold(false, |acc, &x| acc || x)))
            }
            _ => unreachable!(),
        }
    }

    fn eval_cons(args: Vec<Rc<Expr>>) -> EvalRes {
        let head = args[0].clone();
        let tail = args[1].clone();
        Ok(Expr::new_pair(head, tail))
    }

    fn eval_head_tail(&self, args: Vec<Rc<Expr>>) -> EvalRes {
        match *args[0] {
            Expr::Pair(ref head, ref tail) => {
                match *self {
                    BuiltinProc::Head => Ok(head.clone()),
                    BuiltinProc::Tail => Ok(tail.clone()),
                    _ => unreachable!(),
                }
            }
            ref res @ _ => {
                Err(EvalErr::TypeErr {
                    expected: Type::Pair,
                    found: res.get_type(),
                })
            }
        }
    }

    fn eval_print(args: Vec<Rc<Expr>>) -> EvalRes {
        for arg in args {
            println!("{}", arg);
        }
        Ok(Expr::new_nil())
    }

    fn eval_error(args: Vec<Rc<Expr>>) -> EvalRes {
        Err(EvalErr::LogicError(args))
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
            BuiltinProc::Eval => write!(f, "builtin.eval"),
            BuiltinProc::IsDefined => write!(f, "builtin.defined?"),
            BuiltinProc::IsQuote => write!(f, "builtin.quote?"),
            BuiltinProc::IsNumber => write!(f, "builtin.number?"),
            BuiltinProc::IsBoolean => write!(f, "builtin.boolean?"),
            BuiltinProc::IsLambda => write!(f, "builtin.lambda?"),
            BuiltinProc::IsPair => write!(f, "builtin.pair?"),
            BuiltinProc::IsNil => write!(f, "builtin.nil?"),
            BuiltinProc::Print => write!(f, "builtin.print"),
            BuiltinProc::Error => write!(f, "builtin.error"),
        }
    }
}
