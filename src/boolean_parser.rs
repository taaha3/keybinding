use std::iter::Peekable;

use logos::{Logos, Lexer};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t]+")]
enum Token<'src> {
    #[regex(r#"[a-zA-Z0-9_<>\-\./\\:\*\?\+\[\]\^,#@;"%\$\p{L}-]+"#)]
    Str(&'src str),
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token("!")]
    Neg,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

#[derive(Debug)]
enum Expr<'src> {
    Str(&'src str),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Eq(Box<Self>, &'src str),
    NotEq(Box<Self>, &'src str),
    Neg(Box<Self>),
}


impl<'src> Expr<'src> {
    fn eval(&self) -> bool {
        match self {
            Self::Str(string) => get_bool_value(string),
            Self::And(lhs, rhs) => lhs.eval() && rhs.eval(),
            Self::Or(lhs, rhs) => lhs.eval() || rhs.eval(),
            Self::Neg(expr) => !expr.eval(),
            // change below
            Self::Eq(lhs, rhs) => {
                if let Expr::Str(string) = &**lhs {
                    get_str_value(string) == *rhs
                } else {
                    panic!("lhs of == isnt value");
                }
            }
            Self::NotEq(lhs, rhs) => todo!(),
        }
    }
}

type PLexer<'src> = Peekable<Lexer<'src, Token<'src>>>;

fn parse<'src>(input: &str) -> bool {
    let mut lexer = Token::lexer(input).peekable();
    parse_expr(&mut lexer).eval()
}

fn parse_expr<'src>(lexer: &mut PLexer<'src>) -> Expr<'src> {
    let mut lhs = parse_sub_expr(lexer);

    while let Some(token) = lexer.peek() {
        match token {
            Ok(Token::And) => {
                lexer.next();
                let rhs = parse_expr(lexer);
                lhs = Expr::And(Box::new(lhs), Box::new(rhs));
            }
            Ok(Token::Or    ) => {
                lexer.next();
                let rhs = parse_expr(lexer);
                lhs = Expr::Or(Box::new(lhs), Box::new(rhs));
            }
            Ok(Token::Eq) => {
                lexer.next();
                let rhs = parse_expr(lexer);
                lhs = Expr::Eq(Box::new(lhs), if let Expr::Str(string) = rhs {
                    string
                } else {
                    panic!("lhs of == isnt value");
                });
            }
            Ok(Token::NotEq) => {
                todo!()
            }
            _ => break,
        }
    }

    lhs
}

fn parse_sub_expr<'src>(lexer: &mut PLexer<'src>) -> Expr<'src> {
    match lexer.next().unwrap().unwrap() {
        Token::Str(string) => Expr::Str(string),
        Token::Neg => {
            let neg_expr = parse_expr(lexer);
            Expr::Neg(Box::new(neg_expr))
        }
        Token::LParen => {
            let nested_expr = parse_expr(lexer);
            // TODO: change asserteq?
            assert_eq!(lexer.next().unwrap().unwrap(), Token::RParen);
            nested_expr
        }
        token => panic!("invalid token: {:?}", token),
    }
}

// test
// TODO: actual function here?
// need to use hashmap
fn get_bool_value(string: &str) -> bool {
    match string {
        "truetest" => true,
        "falsetest" => false,
        _ => unreachable!(),
    }
}

fn get_str_value(string: &str) -> &str {
    match string {
        "stay" => "hard",
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let parsed = parse("(truetest || falsetest) && stay == hard");

        assert_eq!(parsed, true);
    }
}