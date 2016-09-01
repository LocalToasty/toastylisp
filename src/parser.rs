use std::str;
use std::char;
use std::rc::Rc;
use expression::Expr;
use nom::*;

named!(pub parse_root<&str, Rc<Expr> >,
       map!(many1!(expr),
            Expr::new_sequence));

named!(expr<&str, Rc<Expr> >,
       preceded!(opt!(multispace),
                 alt_complete!(quote |
                               placeholder |
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
named!(end<&str, &str>,
       peek!(alt!(multispace |
                  tag_s!("(") | tag_s!(")") |
                  tag_s!(".") |
                  tag_s!("'"))));

named!(quote<&str, Rc<Expr> >,
       chain!(tag_s!("'") ~ opt!(multispace) ~
              quotation: expr,
              || Expr::new_quote(quotation)));

named!(placeholder<&str, Rc<Expr> >,
       map!(terminated!(tag_s!("_"), end),
            |_| Expr::new_placeholder()));

named!(if_expr<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("if") ~ end ~
              pred: expr ~
              cons: expr ~
              alt: expr ~ opt!(multispace) ~
              tag_s!(")"),
              || Expr::new_if(pred, cons, alt)));

named!(cond<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("cond") ~ end ~
              cases: many0!(chain!(opt!(multispace) ~
                                   tag_s!("(") ~
                                   pred: expr ~
                                   cons: expr ~ opt!(multispace) ~
                                   tag_s!(")"),
                                   || (pred, cons))) ~
              opt!(multispace) ~
              tag_s!(")"),
              || Expr::new_cond(cases)));

named!(let_expr<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("let") ~ opt!(multispace) ~
              tag_s!("(") ~ opt!(multispace) ~
              defs: many0!(chain!(tag_s!("(") ~
                                  opt!(multispace) ~
                                  symb: ident ~
                                  val: expr ~ opt!(multispace) ~
                                  tag_s!(")") ~ opt!(multispace),
                                  || (String::from(symb), val))) ~ opt!(multispace) ~
              tag_s!(")") ~
              body: expr ~ opt!(multispace) ~
              tag_s!(")"),
              || Expr::new_let(defs, body)));

named!(sequence<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("sequence") ~ end ~
              seq: parse_root ~ opt!(multispace) ~
              tag_s!(")"),
              || seq));

named!(define<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("define") ~ multispace ~
              symb: ident ~
              val: expr ~ opt!(multispace) ~
              tag_s!(")"),
              || Expr::new_define(String::from(symb), val)));

named!(lambda<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~ opt!(multispace) ~
              tag_s!("lambda") ~ opt!(multispace) ~
              args: param_list ~
              body: expr ~ opt!(multispace) ~
              tag_s!(")"),
              || Expr::new_lambda(args.0.clone(), args.1.clone(), body, None)));

named!(param_list<&str, (Vec<String>, bool)>,
       chain!(tag_s!("(") ~
              elems: many0!(
                  preceded!(opt!(multispace), map!(ident, String::from))) ~
              variadic: opt!(tag_s!("...")) ~ opt!(multispace) ~
              tag_s!(")"),
              || (elems, variadic.is_some())));

named!(list<&str, Rc<Expr> >,
       chain!(tag_s!("(") ~
              elems: many0!(expr) ~
              opt!(multispace) ~
              tag_s!(")"),
              || {
                  elems.iter().rev().fold(Expr::new_nil(), |xs, x| Expr::new_pair(x.clone(), xs))
              }));

named!(literal<&str, Rc<Expr> >,
       alt_complete!(map!(number, |n| Expr::new_number(n)) |
                     map!(boolean, |b| Expr::new_boolean(b)) |
                     map!(tag_s!("#nil"), |_| Expr::new_nil()) |
                     map!(character, |c| Expr::new_character(c))));

named!(number<&str, i32>,
       chain!(sign: opt!(alt!(tag_s!("+") |
                              tag_s!("-"))) ~
              abs: alt_complete!(map_res!(preceded!(alt!(tag_s!("0x") | tag_s!("0X")),
                                                    is_a_s!("0123456789abcdefABCDEF")),
                                 |s| i32::from_str_radix(s, 16)) |
                        map_res!(digit, |s| i32::from_str_radix(s, 10))),
              || match sign {
                  Some("-") => 0 - abs,
                  _ => abs
              }));

named!(character<&str, char>,
       preceded!(tag_s!("#"),
                 alt_complete!(
                     map!(tag_s!("\\t"), |_| '\t') |
                     map!(tag_s!("\\n"), |_| '\n') |
                     map!(tag_s!("\\r"), |_| '\r') |
                     map!(tag_s!("\\\\"), |_| '\\') |
                     map!(tag_s!("\\_"), |_| ' ') |
                     map_opt!(map_res!(preceded!(tag_s!("\\u"), is_a_s!("0123456789abcdefABCDEF")),
                                       |s| u32::from_str_radix(s, 16)),
                              char::from_u32) |
                     char_s
                 )));

fn char_s(input: &str) -> IResult<&str, char> {
    let mut iter = input.char_indices();

    let first = match iter.next() {
        Some((_, c)) => c,
        None => return IResult::Incomplete(Needed::Size(1)),
    };

    let char_boundary = match iter.next() {
        Some((boundary, _)) => boundary,
        None => return IResult::Done(&input[0..0], first),
    };

    let (_, rest) = input.split_at(char_boundary);
    IResult::Done(rest, first)
}

named!(boolean<&str, bool>,
       alt!(map!(tag_s!("#true"), |_| true) |
            map!(tag_s!("#false"), |_| false)));

named!(symbol<&str, Rc<Expr> >,
       map!(ident, Expr::new_symbol));

fn is_not_end(c: char) -> bool {
    !(c.is_whitespace() || c == '(' || c == ')' || c == '\'' || c == '.')
}

/// Parses an identifier.
///
/// An identifier consists of one or more characters which are not whitespace, '(', ')', or ''' .
/// The first character furthermore must not be a numeric one.
named!(ident<&str, &str>,
       take_while1_s!(is_not_end));

/// Removes all comments from a string.
pub fn remove_comments(input: &str) -> String {
    let mut res = String::new();

    for line in input.lines() {
        for b in line.chars() {
            if b == ';' {
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
                   expr("'foo").unwrap().1);
    }

    #[test]
    fn parse_if() {
        assert_eq!(Expr::new_if(Expr::new_symbol("pred"),
                                Expr::new_symbol("cons"),
                                Expr::new_symbol("alt")),
                   expr("(if pred cons alt)").unwrap().1);
    }

    #[test]
    fn parse_let() {
        assert_eq!(Expr::new_let(vec![(String::from("x"), Expr::new_number(1))],
                                 Expr::new_symbol("x")),
                   expr("(let ((x 1)) x)").unwrap().1);
    }

    #[test]
    fn parse_define() {
        assert_eq!(Rc::new(Expr::Define(String::from("foo"), Expr::new_symbol("nil"))),
                   expr("(define foo nil)").unwrap().1);
    }

    #[test]
    fn parse_list() {
        assert_eq!(Rc::new(Expr::Pair(Expr::new_symbol("foo"),
                                      Rc::new(Expr::Pair(Expr::new_symbol("nil"),
                                                         Expr::new_nil())))),
                   expr("(foo nil)").unwrap().1);
    }

    #[test]
    fn parse_number() {
        // decimal
        assert_eq!(Expr::new_number(42), expr("42").unwrap().1);
        // signed
        assert_eq!(Expr::new_number(1337), expr("+1337").unwrap().1);
        assert_eq!(Expr::new_number(-1337), expr("-1337").unwrap().1);

        // hex
        assert_eq!(Expr::new_number(0xdead), expr("0xdead").unwrap().1);
        assert_eq!(Expr::new_number(0xbeef), expr("0XBEEF").unwrap().1);
        // signed
        assert_eq!(Expr::new_number(-0xcafe), expr("-0xcafe").unwrap().1);
    }

    #[test]
    fn parse_character() {
        // ascii chars
        assert_eq!(Expr::new_character('a'), expr("#a").unwrap().1);
        assert_eq!(Expr::new_character('_'), expr("#_").unwrap().1);
        assert_eq!(Expr::new_character('#'), expr("##").unwrap().1);

        // special characters
        assert_eq!(Expr::new_character('ä'), expr("#ä").unwrap().1);
        assert_eq!(Expr::new_character('ß'), expr("#ß").unwrap().1);
        assert_eq!(Expr::new_character('λ'), expr("#λ").unwrap().1);
        assert_eq!(Expr::new_character('💖'), expr("#💖").unwrap().1);

        // escape sequences
        assert_eq!(Expr::new_character('\t'), expr("#\\t").unwrap().1);
        assert_eq!(Expr::new_character('\n'), expr("#\\n").unwrap().1);
        assert_eq!(Expr::new_character('\r'), expr("#\\r").unwrap().1);
        assert_eq!(Expr::new_character('\\'), expr("#\\\\").unwrap().1);
        assert_eq!(Expr::new_character(' '), expr("#\\_").unwrap().1);

        // unicode escapes
        assert_eq!(Expr::new_character('\u{2764}'), expr("#\\u2764").unwrap().1);
    }

    #[test]
    fn parse_boolean() {
        assert_eq!(Expr::new_boolean(true), expr("#true").unwrap().1);
        assert_eq!(Expr::new_boolean(false), expr("#false").unwrap().1);
    }

    #[test]
    fn parse_nil() {
        assert_eq!(Expr::new_nil(), expr("#nil").unwrap().1);
    }

    #[test]
    fn parse_symbol() {
        // simple symbol
        assert_eq!(Expr::new_symbol("cake"), expr("cake").unwrap().1);

        // symbol with special characters
        assert_eq!(Expr::new_symbol("R2-D2?"), expr("R2-D2?").unwrap().1);
    }
}
