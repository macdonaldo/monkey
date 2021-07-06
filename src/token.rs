use std::fmt;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String),
    INT(String),
    STRING(String),

    // Operators
    PLUS,
    ASSIGN,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,
    EQUAL,
    NOTEQUAL,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::ILLEGAL => write!(f, "Token::Illegal"),
            Token::EOF => write!(f, "Token::EOF"),

            // Identifiers + literals
            Token::IDENT(x) => write!(f, "{}", x),
            Token::INT(x) => write!(f, "{}", x),
            Token::STRING(x) => write!(f, "{}", x),

            // Operators
            Token::PLUS => write!(f, "+"),
            Token::ASSIGN => write!(f, "="),
            Token::MINUS => write!(f, "-"),
            Token::BANG => write!(f, "!"),
            Token::ASTERISK => write!(f, "*"),
            Token::SLASH => write!(f, "/"),

            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::EQUAL => write!(f, "=="),
            Token::NOTEQUAL => write!(f, "!="),

            // Delimiters
            Token::COMMA => write!(f, ","),
            Token::SEMICOLON => write!(f, ";"),

            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),

            // Keywords
            Token::FUNCTION => write!(f, "fn"),
            Token::LET => write!(f, "let"),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::IF => write!(f, "if"),
            Token::ELSE => write!(f, "else"),
            Token::RETURN => write!(f, "return"),
        }
    }
}
