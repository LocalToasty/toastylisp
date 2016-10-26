use std::rc::Rc;
use std::cell::RefCell;
use expression::{Expr, PartialEvalRes, eval_partially};
use environment::Environment;

#[derive(Debug,PartialEq,Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub variadic: bool,
    pub body: Rc<Expr>,
    pub env: Option<Rc<RefCell<Environment>>>,
}

impl Lambda {
    /// Applies arguments to a procedure.
    ///
    /// If the number of arguments matches the adicity of the procedure, and no placeholders were
    /// passed, the procedure's body is evaluated with the arguments bound to the corresponding
    /// parameters.  In case of a variadic procedure, arguments bound to the variadic element are
    /// composed into a list.
    ///
    /// If too few arguments are supplied, or some of the argumets are placeholders, the function is
    /// "curried", and a new function taking less arguments is returned.  If the function is
    /// variadic and there are less arguments then parameters, the variadic parameter is assumed to
    /// be empty.
    pub fn apply(&self, args: Vec<Rc<Expr>>) -> PartialEvalRes {
        // parameters skipped with place holders
        let mut skipped_params = Vec::new();

        // add arguments to environment
        let lambda_scope = Environment::new_scope(self.env.clone().unwrap());
        {
            let mut borrowed_lambda_scope = lambda_scope.borrow_mut();

            let defs = self.params
                           .iter()
                           .zip(args.iter())
                           .map(|(param, arg)| (param.clone(), arg.clone()));

            for (param, arg) in defs {
                match *arg {
                    Expr::Placeholder => skipped_params.push(param),
                    _ => borrowed_lambda_scope.insert(param, arg),
                };
            }

            if self.variadic {
                if args.len() < self.params.len() - 1 {
                    // function is curried, without defining any element in the argument list
                    borrowed_lambda_scope.insert(self.params[self.params.len() - 1].clone(),
                                                 Expr::new_nil());
                } else {
                    let mut placeholder_no = 0;

                    // collect remaining arguments
                    let vararg_list = args[self.params.len() - 1..]
                                          .iter()
                                          .cloned()
                                          .map(|x| {
                                              match *x {
                                                  Expr::Placeholder => {
                                                      let name = format!("_tl_placeholder_{}",
                                                                         placeholder_no);
                                                      placeholder_no += 1;
                                                      Rc::new(Expr::Symbol(name))
                                                  }
                                                  _ => x,
                                              }
                                          })
                                          .rev()
                                          .fold(Expr::new_nil(), |xs, x| Expr::new_pair(x, xs));

                    if placeholder_no == 0 {
                        // none of the arguments in the variadic part were skipped with
                        // placeholders; bind normally
                        borrowed_lambda_scope.insert(self.params[self.params.len() - 1].clone(),
                                                     vararg_list);
                    } else {
                        // if any of of the variadic parameters is a placeholder, the whole function
                        // call is wrapped in another lambda

                        // insert placeholder params
                        for i in 0..placeholder_no {
                            skipped_params.push(format!("_tl_placeholder_{}", i));
                        }

                        let arg_list = args[0..self.params.len() - 1]
                                           .iter()
                                           .zip(self.params.iter())
                                           .map(|(arg, param)| {
                                               match **arg {
                                                   Expr::Placeholder => {
                                                       Rc::new(Expr::Symbol(param.clone()))
                                                   }
                                                   _ => arg.clone(),
                                               }
                                           })
                                           .rev()
                                           .fold(vararg_list, |xs, x| Expr::new_pair(x, xs));

                        let res =
                            Expr::new_lambda(skipped_params,
                                             false,
                                             Expr::new_pair(Rc::new(Expr::Lambda(self.clone())),
                                                            arg_list),
                                             self.env.clone());
                        return PartialEvalRes::Done(res);
                    }
                }
            }
        }

        if skipped_params.is_empty() &&
           (args.len() == self.params.len() ||
            (self.variadic && args.len() >= self.params.len() - 1)) {
            // correct amount of arguments, evaluate body
            eval_partially(&self.body, &lambda_scope)
        } else if !self.variadic {
            // too few arguments were given; return curried lambda
            let remaining_params = skipped_params.iter()
                                                 .chain(self.params[args.len()..].iter())
                                                 .cloned()
                                                 .collect();
            PartialEvalRes::Done(Expr::new_lambda(remaining_params,
                                                  false,
                                                  self.body.clone(),
                                                  Some(lambda_scope)))
        } else {
            PartialEvalRes::Done(Expr::new_lambda(skipped_params,
                                                  false,
                                                  self.body.clone(),
                                                  Some(lambda_scope)))
        }
    }
}
