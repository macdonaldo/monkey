extern crate monkey;

use std::vec;

use monkey::lexer::*;
use monkey::token::*;

#[test]
fn lexer_1_next_token() {
    let input: &str = "=+(){},;";
    let input: Vec<char> = input.chars().collect();

    let expected_tokens = vec![
        Token::ASSIGN,
        Token::PLUS,
        Token::LPAREN,
        Token::RPAREN,
        Token::LBRACE,
        Token::RBRACE,
        Token::COMMA,
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);

    for t in &expected_tokens {
        let tok = l.next_token();
        assert_eq!(*t, tok);
    }
}

#[test]
fn lexer_2_next_token() {
    let input: &str = "let five = 5;
    let ten = 10;
    
    let add = fn(x, y) {
        x + y;
    };
    
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    
    if (5 < 10) {
        return true;
    } else {
        return false;
    }
    
    10 == 10;
    10 != 9;
    10 <= 9;
    10 >= 9;
    ";
    let input: Vec<char> = input.chars().collect();

    let expected_tokens = vec![
        Token::LET,
        Token::IDENT(String::from("five")),
        Token::ASSIGN,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("ten")),
        Token::ASSIGN,
        Token::INT(String::from("10")),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("add")),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT(String::from("x")),
        Token::COMMA,
        Token::IDENT(String::from("y")),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT(String::from("x")),
        Token::PLUS,
        Token::IDENT(String::from("y")),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("result")),
        Token::ASSIGN,
        Token::IDENT(String::from("add")),
        Token::LPAREN,
        Token::IDENT(String::from("five")),
        Token::COMMA,
        Token::IDENT(String::from("ten")),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERISK,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::INT(String::from("5")),
        Token::LT,
        Token::INT(String::from("10")),
        Token::GT,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT(String::from("5")),
        Token::LT,
        Token::INT(String::from("10")),
        Token::RPAREN,
        Token::LBRACE,
        Token::RETURN,
        Token::TRUE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::ELSE,
        Token::LBRACE,
        Token::RETURN,
        Token::FALSE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::INT(String::from("10")),
        Token::EQUAL,
        Token::INT(String::from("10")),
        Token::SEMICOLON,
        Token::INT(String::from("10")),
        Token::NOTEQUAL,
        Token::INT(String::from("9")),
        Token::SEMICOLON,
        Token::INT(String::from("10")),
        Token::LT,
        Token::ASSIGN,
        Token::INT(String::from("9")),
        Token::SEMICOLON,
        Token::INT(String::from("10")),
        Token::GT,
        Token::ASSIGN,
        Token::INT(String::from("9")),
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);

    for t in &expected_tokens {
        let tok = l.next_token();
        assert_eq!(*t, tok);
    }
}
