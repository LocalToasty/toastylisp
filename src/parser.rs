use std::str;
use std::str::FromStr;
use std::rc::Rc;
use expression::Expr;
use nom::*;

named!(pub parse_root<Rc<Expr> >,
       map!(many1!(expr),
            Expr::new_sequence));

named!(expr<Rc<Expr> >,
       preceded!(opt!(multispace),
                 alt_complete!(quote |
                               if_expr |
                               cond |
                               let_expr |
                               sequence |
                               define |
                               lambda |
                               list |
                               literal |
                               symbol)));

/// Recognizes the end of an expression, without affecting the parser.
named!(end<char>,
       peek!(alt!(map!(multispace, |_| ' ') |
                  char!('(') |
                  char!(')') |
                  char!('\''))));

named!(quote<Rc<Expr> >,
       chain!(char!('\'') ~ opt!(multispace) ~
              quotation: expr,
              || Expr::new_quote(quotation)));

named!(if_expr<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("if") ~ end ~
              pred: expr ~
              cons: expr ~
              alt: expr ~ opt!(multispace) ~
              char!(')'),
              || Expr::new_if(pred, cons, alt)));

named!(cond<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("cond") ~ end ~
              cases: many0!(chain!(opt!(multispace) ~
                                   char!('(') ~
                                   pred: expr ~
                                   cons: expr ~ opt!(multispace) ~
                                   char!(')'),
                                   || (pred, cons))) ~
              opt!(multispace) ~
              char!(')'),
              || Expr::new_cond(cases)));

named!(let_expr<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("let") ~ opt!(multispace) ~
              char!('(') ~ opt!(multispace) ~
              defs: many0!(chain!(char!('(') ~
                                  opt!(multispace) ~
                                  symb: ident ~
                                  val: expr ~ opt!(multispace) ~
                                  char!(')') ~ opt!(multispace),
                                  || (String::from(symb), val))) ~ opt!(multispace) ~
              char!(')') ~
              body: expr ~ opt!(multispace) ~
              char!(')'),
              || Expr::new_let(defs, body)));

named!(sequence<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("sequence") ~ end ~
              seq: parse_root ~ opt!(multispace) ~
              char!(')'),
              || seq));

named!(define<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("define") ~ multispace ~
              symb: ident ~
              val: expr ~ opt!(multispace) ~
              char!(')'),
              || Expr::new_define(String::from(symb), val)));

named!(lambda<Rc<Expr> >,
       chain!(char!('(') ~ opt!(multispace) ~
              tag!("lambda") ~ opt!(multispace) ~
              args: str_list ~
              body: expr ~ opt!(multispace) ~
              char!(')'),
              || Expr::new_lambda(args, body, None)));

named!(str_list<Vec<String> >,
       chain!(char!('(') ~
              elems: many0!(
                  preceded!(opt!(multispace), map!(ident, String::from))) ~
              opt!(multispace) ~
              char!(')'),
              || elems));

named!(list<Rc<Expr> >,
       chain!(char!('(') ~
              mut elems: many0!(expr) ~
              opt!(multispace) ~
              char!(')'),
              || {
                  let mut list = Expr::new_nil();
                  while let Some(elem) = elems.pop() {
                      list = Expr::new_pair(elem, list);
                  }
                  list
              }));

named!(literal<Rc<Expr> >,
       alt!(map!(number, |n| Expr::new_number(n)) |
            map!(boolean, |b| Expr::new_boolean(b)) |
            map!(tag!("#nil"), |_| Expr::new_nil())));

named!(positive_number<i32>,
       map_res!(map_res!(digit,
                         str::from_utf8),
                FromStr::from_str));

named!(number<i32>,
       chain!(sign: opt!(alt!(char!('+') |
                              char!('-'))) ~
              abs: positive_number,
              || match sign {
                  Some('-') => 0 - abs,
                  _ => abs
              }));

named!(boolean<bool>,
       alt!(map!(tag!("#true"), |_| true) |
            map!(tag!("#false"), |_| false)));


named!(symbol<Rc<Expr> >,
       map!(ident, Expr::new_symbol));

fn is_not_end(input: u8) -> bool {
    let c = input as char;

    !(c.is_whitespace() || c == '(' || c == ')' || c == '\'')
}

/// Parses an identifier.
///
/// An identifier consists of one or more characters which are not whitespace, '(', ')', or ''' .
/// The first character furthermore must not be a numeric one.
named!(ident<&str>,
       map_res!(take_while1!(is_not_end),
                str::from_utf8));

/// Removes all comments from a string.
pub fn remove_comments(input: &str) -> Vec<u8> {
    let mut res = Vec::new();

    for line in input.lines() {
        for b in line.bytes() {
            if b == ';' as u8 {
                break;
            }

            res.push(b);
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use expression::Expr;
    use super::expr;

    #[test]
    fn parse_quote() {
        assert_eq!(Expr::new_quote(Expr::new_symbol("foo")),
                   expr(b"'foo").unwrap().1);
    }

    #[test]
    fn parse_if() {
        assert_eq!(Expr::new_if(Expr::new_symbol("pred"),
                                Expr::new_symbol("cons"),
                                Expr::new_symbol("alt")),
                   expr(b"(if pred cons alt)").unwrap().1);
    }

    #[test]
    fn parse_let() {
        assert_eq!(Expr::new_let(vec![(String::from("x"), Expr::new_number(1))],
                                 Expr::new_symbol("x")),
                   expr(b"(let ((x 1)) x)").unwrap().1);
    }

    #[test]
    fn parse_define() {
        assert_eq!(Rc::new(Expr::Define(String::from("foo"), Expr::new_symbol("nil"))),
                   expr(b"(define foo nil)").unwrap().1);
    }

    #[test]
    fn parse_list() {
        assert_eq!(Rc::new(Expr::Pair(Expr::new_symbol("foo"),
                                      Rc::new(Expr::Pair(Expr::new_symbol("nil"),
                                                         Expr::new_nil())))),
                   expr(b"(foo nil)").unwrap().1);
    }

    #[test]
    fn parse_number() {
        assert_eq!(Rc::new(Expr::Number(42)), expr(b"42").unwrap().1);
        assert_eq!(Rc::new(Expr::Number(-7)), expr(b"-7").unwrap().1);
    }

    #[test]
    fn parse_symbol() {
        // simple symbol
        assert_eq!(Expr::new_symbol("cake"), expr(b"cake").unwrap().1);

        // symbol with special characters
        assert_eq!(Expr::new_symbol("R2-D2?"), expr(b"R2-D2?").unwrap().1);
    }
}
