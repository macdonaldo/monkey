use crate::token::*;
use std::collections::HashMap;

pub struct Lexer {
    input: Vec<char>,
    curr_position: usize, // current position in input(points to current char)
    next_position: usize, // current read position (after current char)
    character: char,      // current character under examination
}

impl Lexer {
    pub fn new(input: Vec<char>) -> Lexer {
        let mut l = Lexer {
            input,
            curr_position: 0,
            next_position: 0,
            character: '\0',
        };

        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.next_position >= self.input.len() {
            self.character = '\0';
        } else {
            self.character = self.input[self.next_position];
        }
        self.curr_position = self.next_position;
        self.next_position += 1;
    }

    fn peek_char(&self) -> char {
        let mut next_char = '\0';
        if self.next_position < self.input.len() {
            next_char = self.input[self.next_position];
        }
        next_char
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.character {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::EQUAL
                } else {
                    Token::ASSIGN
                }
            }
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '+' => Token::PLUS,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '\0' => Token::EOF,
            '-' => Token::MINUS,
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NOTEQUAL
                } else {
                    Token::BANG
                }
            }
            '*' => Token::ASTERISK,
            '/' => Token::SLASH,
            '<' => Token::LT,
            '>' => Token::GT,
            '"' => self.read_string(),
            _ => {
                if self.is_letter() {
                    return self.read_identifier();
                } else if self.character.is_digit(10) {
                    return self.read_number();
                } else {
                    Token::ILLEGAL
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.curr_position;
        loop {
            if !self.is_letter() && !self.character.is_digit(10) {
                break;
            }

            self.read_char();
        }

        let identifier: String = self.input[start..self.curr_position].iter().collect();
        let kw_map: HashMap<&str, Token> = [
            ("fn", Token::FUNCTION),
            ("let", Token::LET),
            ("if", Token::IF),
            ("else", Token::ELSE),
            ("true", Token::TRUE),
            ("false", Token::FALSE),
            ("return", Token::RETURN),
        ]
        .iter()
        .cloned()
        .collect();

        let tok = match kw_map.get(&identifier.as_str()) {
            Some(kw) => (*kw).clone(),
            None => Token::IDENT(identifier),
        };

        tok
    }

    const fn is_letter(&self) -> bool {
        self.character.is_ascii_alphabetic() || self.character == '_'
    }

    fn skip_whitespace(&mut self) {
        loop {
            if !matches!(self.character, ' ' | '\t' | '\n' | '\r') {
                break;
            }
            self.read_char();
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.curr_position;
        loop {
            if !self.character.is_digit(10) {
                break;
            }
            self.read_char();
        }
        let num_str = self.input[start..self.curr_position]
            .iter()
            .collect::<String>();
        Token::INT(num_str)
    }

    fn read_string(&mut self) -> Token {
        let start = self.curr_position + 1; // skip opening double-quote
        loop {
            self.read_char();
            if matches!(self.character, '"' | '\0') {
                break;
            }
        }
        let string = self.input[start..self.curr_position]
            .iter()
            .collect::<String>();
        Token::STRING(string)
    }
}
