#[macro_use]
extern crate nom;

use std::rc::Rc;
use std::cell::RefCell;
use expression::{Expr, eval};
use environment::Environment;
use std::io::prelude::*;
use std::fs::File;
use std::env;

mod expression;
mod builtin;
mod environment;
mod parser;

fn main() {
    let mut args = env::args();
    args.next();
    for arg in args {
        if arg == "-v" || arg == "--verbose" {
            expression::set_verbose(true);
        } else {
            let mut f = File::open(arg).unwrap();
            let mut buffer = String::new();
            let _ = f.read_to_string(&mut buffer);
            parse_and_eval(buffer.trim());
        }
    }
}

fn parse_and_eval(input: &str) -> Rc<Expr> {
    let sanatized_input = parser::remove_comments(input);
    let prog = parser::parse_root(&sanatized_input[..]).unwrap().1;
    let env = Environment::new();
    eval(&prog, &Rc::new(RefCell::new(env))).unwrap()
}

#[cfg(test)]
mod tests {
    use expression::Expr;
    use super::parse_and_eval;

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn comments() {
        let prog = "(define x ;hello world \n\
                            1) ; define x as 1 \n\
                    x";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));

        let prog = "(define x ;hello world \n\
                            1) ; define x as 1 \n\
                    x \n\
                    ; some trailing comments \n\

                    ; which should generate some trailing whitespace";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));
    }

    #[test]
    fn define() {
        let prog = "(define x 1) x";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));
    }

    #[test]
    fn simple_lambda() {
        let prog = "((lambda (x) x) 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(3));

        let prog = "((lambda (x y) (cons x y)) 3 4)";
        assert_eq!(*parse_and_eval(prog),
                   Expr::Pair(Expr::new_number(3), Expr::new_number(4)));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn lambda_with_list_arg() {
        let prog = "(define second (lambda (xs) (head (tail xs)))) \
                    (second '(1 2 3))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn recursion() {
        let prog = "(define count \
                      (lambda (down up) \
                        (if (= down 0) \
                            up \
                            (count (- down 1) (+ up 1))))) \
                    (count 3 0)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(3));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn capture() {
        let prog = "(define f \
                      (let ((x 1)) \
                        (lambda () x))) \
                    (f)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));
    }

    #[test]
    fn builtin() {
        let prog = "(+ 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(5));

        let prog = "(- 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(-1));

        let prog = "(* 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(6));

        let prog = "(/ 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(0));

        let prog = "(mod 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));

        let prog = "(< 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(< 3 2)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(> 2 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(> 3 2)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(and #true #false)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(and #true #true)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(or #true #false)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(or #false #false)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(not #false)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(not #true)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(cons 1 2)";
        assert_eq!(*parse_and_eval(prog),
                   Expr::Pair(Expr::new_number(1), Expr::new_number(2)));

        let prog = "(head (cons 1 2))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));

        let prog = "(tail (cons 1 2))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));

        let prog = "(defined? 'x)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(define x 1) (defined? 'x)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(number? 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(number? #nil)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(boolean? #true)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(boolean? #false)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(boolean? #nil)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(quote? ''foo)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(quote? 'foo)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(quote? 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(lambda? (lambda (x) (x)))";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(lambda? 0)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(pair? (cons 1 2))";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(pair? 0)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));

        let prog = "(nil? #nil)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(nil? '())";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(true));

        let prog = "(nil? 0)";
        assert_eq!(*parse_and_eval(prog), Expr::Boolean(false));
    }

    #[test]
    fn redefine_builtin() {
        let prog = "(define + -) 1";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1));
    }

    #[test]
    fn currying() {
        // single curry
        let prog = "(((lambda (x y) y) 1) 2)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));

        // double curry
        let prog = "(((lambda (x y z) z) 1) 2) 3)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(3));

        // currying with place holders
        let prog = "((/ _ 2) 8)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(4));
    }

    #[test]
    #[should_panic]
    fn too_many_placeholders() {
        let prog = "(+ _ _ _)";
        parse_and_eval(prog);
    }

    #[test]
    fn eval() {
        let prog = "(eval '(+ 1 (* 2 3)))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(7));
    }


    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn postponed_definition() {
// define the value of bar after the one of foo
        let prog = "(define foo (lambda () bar)) \
                    (define bar 1337) \
                    (foo)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1337));

// same for let
        let prog = "(let ((foo (lambda () bar)) \
                          (bar 1337)) \
                      (foo))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(1337));
    }

    #[test]
    fn quoting() {
        let prog = "'(foo bar baz)";
        assert_eq!(parse_and_eval(prog),
                   Expr::new_list(vec![Expr::new_symbol("foo"),
                                       Expr::new_symbol("bar"),
                                       Expr::new_symbol("baz")]));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn length() {
        let prog = "(define length \
                      (lambda (xs) \
                        (if (nil? xs) \
                            0 \
                          (+ 1 (length (tail xs)))))) \
                    (length '(1 2 3 4 5))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(5));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn factorial() {
        let prog = "(define fac \
                      (lambda (x) \
                        (if (= x 0) \
                            1 \
                          (* x (fac (- x 1)))))) \
                    (fac 5)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(120));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn gcd() {
        let prog = "(define gcd \
                      (lambda (a b) \
                        (if (= b 0) \
                            a \
                          (gcd b (mod a b))))) \
                    (gcd 20 6)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn fibonacci() {
        let prog = "(define fib \
                      (lambda (n) \
                        (cond ((= n 0) 0) \
                              ((= n 1) 1) \
                              (#true (+ (fib (- n 1)) (fib (- n 2))))))) \
                    (fib 10)";
        assert_eq!(*parse_and_eval(prog), Expr::Number(55));
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn own_list() {
        let prog = "(define own-cons \
                      (lambda (a b) \
                        (lambda (left) \
                          (if left a b)))) \
                    (define own-head (lambda (xs) (xs #true))) \
                    (define own-tail (lambda (xs) (xs #false))) \
                    (define list (own-cons 1 (own-cons 2 (own-cons 3 #nil)))) \
                    (own-head (own-tail list))";
        assert_eq!(*parse_and_eval(prog), Expr::Number(2));
    }

    #[test]
    #[should_panic]
    fn error() {
        let prog = "(error '(this should fail))";
        parse_and_eval(prog);
    }
}
