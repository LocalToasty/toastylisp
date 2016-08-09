#[macro_use]
extern crate nom;

use std::rc::Rc;
use expression::{Expr, eval};
use environment::Environment;
use std::io::prelude::*;
use std::fs::File;
use std::env;

mod expression;
mod intrinsic;
mod environment;
mod parser;

fn main() {
    let mut args = env::args();
    args.next();
    for arg in args {
        let mut f = File::open(arg).unwrap();
        let mut buffer = String::new();
        let _ = f.read_to_string(&mut buffer);
        parse_and_eval(buffer.trim().as_bytes());
    }
}

fn parse_and_eval(input: &[u8]) -> Rc<Expr> {
    let prog = parser::parse_root(input).unwrap().1;
    let env = Environment::new();
    eval(&prog, &Rc::new(env)).unwrap()
}

#[cfg(test)]
mod tests {
    use expression::Expr;
    use super::parse_and_eval;

    #[test]
    fn define() {
        let prog = "(define x 1) x";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(1));
    }

    #[test]
    fn simple_lambda() {
        let prog = "((lambda (x) x) 3)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(3));

        let prog = "((lambda (x y) (cons x y)) 3 4)";
        assert_eq!(*parse_and_eval(prog.as_bytes()),
                   Expr::Pair(Expr::new_number(3), Expr::new_number(4)));
    }

    #[test]
    fn lambda_with_list_arg() {
        let prog = "(define second (lambda (xs) (head (tail xs)))) (second '(1 2 3))";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(2));
    }

    #[test]
    fn recursion() {
        let prog = "(define count (lambda (down up) (if (= down 0) up (count (- down 1) (+ up \
                    1))))) (count 3 0)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(3));
    }

    #[test]
    fn capture() {
        let prog = "(define f (let ((x 1)) (lambda () x))) (f)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(1));
    }

    #[test]
    fn intrinsic() {
        let prog = "(- (/ (+ (* 7 8) 4) 2) 3)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(27));

        let prog = "(cons (+ 1 1) (+ 2 2))";
        assert_eq!(*parse_and_eval(prog.as_bytes()),
                   Expr::Pair(Expr::new_number(2), Expr::new_number(4)));

        let prog = "(mod 5 2)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(1));
    }

    #[test]
    fn currying() {
        // single curry
        let prog = "(((lambda (x y) y) 1) 2)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(2));

        // double curry
        let prog = "(((lambda (x y z) z) 1) 2) 3)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(3));
    }

    #[test]
    fn quoting() {
        let prog = "'(foo bar baz)";
        assert_eq!(parse_and_eval(prog.as_bytes()),
                   Expr::new_list(vec![Expr::new_symbol("foo"),
                                       Expr::new_symbol("bar"),
                                       Expr::new_symbol("baz")]));
    }

    #[test]
    fn length() {
        let prog = "(define length (lambda (xs) (if (nil? xs) 0 (+ 1 (length (tail xs)))))) \
                    (length '(1 2 3 4 5))";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(5));
    }

    #[test]
    fn factorial() {
        let prog = "(define fac (lambda (x) (if (= x 0) 1 (* x (fac (- x 1)))))) (fac 5)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(120));
    }

    #[test]
    fn gcd() {
        let prog = "(define gcd (lambda (a b) (if (= b 0) a (gcd b (mod a b))))) (gcd 20 6)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(2));
    }

    #[test]
    #[should_panic]
    fn clusterfuck() {
        let prog = "(define foo (lambda () foo)) (let ((bar foo) (foo 0)) (bar))";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(0));
    }

    #[test]
    fn fibonacci() {
        let prog = "(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- \
                    n 2))))))) (fib 10)";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(55));
    }

    #[test]
    fn own_list() {
        let prog = "(define own-cons (lambda (a b) (lambda (left?) (if left? a b)))) (define \
                    own-head (lambda (xs) (xs true))) (define own-tail (lambda (xs) (xs nil))) \
                    (define list (own-cons 1 (own-cons 2 (own-cons 3 nil)))) (own-head (own-tail \
                    list))";
        assert_eq!(*parse_and_eval(prog.as_bytes()), Expr::Number(2));
    }
}
