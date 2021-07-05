use crate::token::*;
use std::fmt;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum Statement {
    Let {
        token: Token,
        identifier: Token,
        value: Expression,
    },
    Return {
        token: Token,
        return_value: Expression,
    },
    Expression {
        token: Token,
        expression: Expression,
    },
    Block {
        token: Token, // Token::LBracket {
        statements: Vec<Statement>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Expression {
                token: _,
                expression,
            } => write!(f, "{}", expression),
            Statement::Let {
                token,
                identifier,
                value,
            } => write!(f, "{} {} = {};", token, identifier, value),
            Statement::Return {
                token,
                return_value,
            } => write!(f, "{} {};", token, return_value),
            Statement::Block {
                token: _,
                statements,
            } => {
                let s = statements
                    .iter()
                    .fold(String::new(), |acc, x| acc + format!("{}", x).as_str());
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum Expression {
    Identifier(Token),
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Prefix {
        operator: Token,
        right: Box<Expression>,
    },
    Infix {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    If {
        token: Token, // Token::If
        condition: Box<Expression>,
        consequence: Box<Statement>,         // Statement::Block
        alternative: Option<Box<Statement>>, // Statement::Block
    },
    FunctionLiteral {
        token: Token,                // Token::FUNCTION
        parameters: Vec<Expression>, // Vec<Expression::Identifier>
        body: Box<Statement>,        // Statement::Block
    },
    Call {
        token: Token,              // Token::LPAREN (
        function: Box<Expression>, // Expression::Identifier | Expression::FunctionLiteral
        arguments: Vec<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(token) => write!(f, "{}", token),
            Expression::IntegerLiteral { token: _, value } => write!(f, "{}", value),
            Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                operator,
                left,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::Boolean { token: _, value } => write!(f, "{}", value),
            Expression::If {
                token,
                condition,
                consequence,
                alternative,
            } => {
                let alt_txt = match alternative {
                    Some(b) => format!("{}", **b),
                    _ => String::new(),
                };
                write!(f, "{}{} {}else {}", token, condition, consequence, alt_txt)
            }
            Expression::FunctionLiteral {
                token,
                parameters,
                body,
            } => {
                let mut parameter_list = String::new();
                for (i, e) in parameters.iter().enumerate() {
                    match i == 0 {
                        true => parameter_list.push_str(&format!("{}", e)),
                        _ => parameter_list.push_str(&format!(", {}", e)),
                    };
                }
                write!(f, "{}({}){}", token, parameter_list, *body)
            }
            Expression::Call {
                token: _,
                function,
                arguments,
            } => {
                let mut argument_list = String::new();
                for (i, e) in arguments.iter().enumerate() {
                    match i == 0 {
                        true => argument_list.push_str(&format!("{}", e)),
                        _ => argument_list.push_str(&format!(", {}", e)),
                    };
                }
                write!(f, "{}({})", function, argument_list)
            }
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
    pub errors: Vec<String>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self
            .statements
            .iter()
            .fold(String::new(), |acc, x| acc + format!("{}", x).as_str());
        write!(f, "{}", s)
    }
}
