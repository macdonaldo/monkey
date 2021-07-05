use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
pub enum OpsPrecedence {
    Lowest,
    Equals,      // ==
    LessGreater, // < or >
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(x)
}

pub struct Parser {
    pub lexer: Lexer,
    pub curr_token: Token,
    pub peek_token: Token, // next token
    precedences: HashMap<Token, OpsPrecedence>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let precedences: HashMap<Token, OpsPrecedence> = [
            (Token::EQUAL, OpsPrecedence::Equals),
            (Token::NOTEQUAL, OpsPrecedence::Equals),
            (Token::LT, OpsPrecedence::LessGreater),
            (Token::GT, OpsPrecedence::LessGreater),
            (Token::PLUS, OpsPrecedence::Sum),
            (Token::MINUS, OpsPrecedence::Sum),
            (Token::SLASH, OpsPrecedence::Product),
            (Token::ASTERISK, OpsPrecedence::Product),
            (Token::LPAREN, OpsPrecedence::Call),
        ]
        .iter()
        .cloned()
        .collect();

        let mut p = Parser {
            lexer,
            curr_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,
            precedences,
        };

        // Read 2 tokens so peek_token & curr_token are set
        p.next_token();
        p.next_token();

        p
    }

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
            errors: Vec::new(),
        };

        loop {
            if self.curr_token == Token::EOF {
                break;
            }

            let result = self.parse_statement();
            match result {
                Ok(stmt) => program.statements.push(stmt),
                Err(msg) => {
                    program.errors.push(msg);
                }
            };
            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.curr_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let token = self.curr_token.clone();

        let mut expected_tok = Token::IDENT(String::from("")); // "" - value of the variant not important here
        if !self.expect_peek(&expected_tok) {
            return self.peek_error_stmt(&expected_tok);
        }
        let identifier = self.curr_token.clone();

        expected_tok = Token::ASSIGN;
        if !self.expect_peek(&expected_tok) {
            return self.peek_error_stmt(&expected_tok);
        }

        self.next_token(); // move PAST ==
        let value = self.parse_expression(OpsPrecedence::Lowest)?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let {
            token,
            identifier,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let token = self.curr_token.clone();
        self.next_token();

        let return_value = self.parse_expression(OpsPrecedence::Lowest)?;

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Return {
            token,
            return_value,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let token = self.curr_token.clone();
        let expression = self.parse_expression(OpsPrecedence::Lowest)?;

        // Semicolon is Optional to allow for expressions
        // like 5 + 5 in the REPL
        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::Expression { token, expression })
    }

    fn parse_expression(&mut self, precedence: OpsPrecedence) -> Result<Expression, String> {
        let mut left_expr = self.parse_prefix()?;
        loop {
            if self.peek_token_is(&Token::SEMICOLON)
                || precedence as usize >= self.peek_precedence() as usize
                || self.no_parse_infix_fn(&self.peek_token)
            {
                break;
            }

            self.next_token();

            left_expr = self.parse_infix(Box::new(left_expr))?;
        }

        Ok(left_expr)
    }

    fn curr_token_is(&self, t: Token) -> bool {
        std::mem::discriminant(&self.curr_token) == std::mem::discriminant(&t)
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        std::mem::discriminant(&self.peek_token) == std::mem::discriminant(t)
    }

    fn expect_peek(&mut self, t: &Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn peek_error_stmt(&self, t: &Token) -> Result<Statement, String> {
        Err(format!(
            "Expected next token to be {}, got {} instead",
            t, self.peek_token
        ))
    }

    fn peek_error_expr(&self, t: &Token) -> Result<Expression, String> {
        Err(format!(
            "Expected next token to be {}, got {} instead",
            t, self.peek_token
        ))
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        match &self.curr_token {
            Token::IDENT(_) => self.parse_identifier_expression(),
            Token::INT(num_str) => self.parse_integer_literal_expression(&num_str),
            Token::LPAREN => self.parse_grouped_expressions(),
            Token::BANG | Token::MINUS => self.parse_bang_minus_prefix_expression(),
            Token::TRUE | Token::FALSE => self.parse_boolean_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_literal_expression(),
            _ => Err(format!(
                "no parse prefix function found for {}",
                self.curr_token
            )),
        }
    }

    fn parse_function_literal_expression(&mut self) -> Result<Expression, String> {
        let token = self.curr_token.clone();

        let left_paren = Token::LPAREN;
        if !self.expect_peek(&left_paren) {
            return self.peek_error_expr(&left_paren);
        }

        let parameters = self.parse_function_parameters()?;

        let left_brace = Token::LBRACE;
        if !self.expect_peek(&left_brace) {
            return self.peek_error_expr(&left_brace);
        }

        let body = Box::new(self.parse_block_statement()?);
        Ok(Expression::FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>, String> {
        let mut identifiers = vec![];

        // parameters are optional
        if self.expect_peek(&Token::RPAREN) {
            return Ok(identifiers);
        }

        self.next_token(); // move to first identifier
        identifiers.push(Expression::Identifier(self.curr_token.clone()));

        loop {
            if !self.peek_token_is(&Token::COMMA) {
                break;
            }
            self.next_token(); // move TO comma
            self.next_token(); // move PAST comma

            identifiers.push(Expression::Identifier(self.curr_token.clone()));
        }

        let right_paren = Token::RPAREN;
        if !self.expect_peek(&right_paren) {
            return Err(format!(
                "Expected next token to be {}, got {} instead",
                right_paren, self.peek_token
            ));
        }

        Ok(identifiers)
    }

    fn parse_grouped_expressions(&mut self) -> Result<Expression, String> {
        self.next_token(); // move past (
        let expr = self.parse_expression(OpsPrecedence::Lowest)?;

        let next_token = Token::RPAREN;
        if !self.expect_peek(&next_token) {
            return self.peek_error_expr(&next_token);
        }
        Ok(expr)
    }

    fn parse_boolean_expression(&self) -> Result<Expression, String> {
        Ok(Expression::Boolean {
            token: self.curr_token.clone(),
            value: self.curr_token_is(Token::TRUE),
        })
    }

    fn parse_identifier_expression(&self) -> Result<Expression, String> {
        Ok(Expression::Identifier(self.curr_token.clone()))
    }

    fn parse_integer_literal_expression(&self, num_str: &str) -> Result<Expression, String> {
        match i64::from_str_radix(num_str, 10) {
            Ok(value) => Ok(Expression::IntegerLiteral {
                token: self.curr_token.clone(),
                value,
            }),
            _ => Err(format!("could not pass {} as integer", num_str)),
        }
    }

    fn parse_bang_minus_prefix_expression(&mut self) -> Result<Expression, String> {
        let operator = self.curr_token.clone();
        self.next_token();
        let right = Box::new(self.parse_expression(OpsPrecedence::Prefix)?);

        Ok(Expression::Prefix { operator, right })
    }

    fn peek_precedence(&self) -> OpsPrecedence {
        if let Some(p) = self.precedences.get(&self.peek_token) {
            return *p;
        }
        OpsPrecedence::Lowest
    }

    fn curr_precedence(&self) -> OpsPrecedence {
        if let Some(p) = self.precedences.get(&self.curr_token) {
            return *p;
        }
        OpsPrecedence::Lowest
    }

    fn parse_infix(&mut self, expression: Box<Expression>) -> Result<Expression, String> {
        match &self.curr_token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQUAL
            | Token::NOTEQUAL
            | Token::LT
            | Token::GT => self.parse_infix_expression(expression),
            Token::LPAREN => self.parse_call_expression(expression),
            _ => Err(format!(
                "no parse infix function found for {}",
                self.curr_token
            )),
        }
    }

    fn no_parse_infix_fn(&self, token: &Token) -> bool {
        match token {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASSIGN
            | Token::ASTERISK
            | Token::EQUAL
            | Token::NOTEQUAL
            | Token::LT
            | Token::GT
            | Token::LPAREN => false,
            _ => true,
        }
    }

    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Expression, String> {
        let operator = self.curr_token.clone();
        let precedence = self.curr_precedence();
        self.next_token();
        let right = Box::new(self.parse_expression(precedence)?);

        Ok(Expression::Infix {
            operator,
            left,
            right,
        })
    }

    // if (<condition>) <consequence> else <alternative>
    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        let token = self.curr_token.clone();

        let left_paren = Token::LPAREN;
        if !self.expect_peek(&left_paren) {
            return self.peek_error_expr(&left_paren);
        }
        self.next_token(); // move past (

        let condition = Box::new(self.parse_expression(OpsPrecedence::Lowest)?);
        let right_paren = Token::RPAREN;
        if !self.expect_peek(&right_paren) {
            return self.peek_error_expr(&right_paren);
        }

        let left_brace = Token::LBRACE;
        if !self.expect_peek(&left_brace) {
            return self.peek_error_expr(&left_brace);
        }

        let consequence = Box::new(self.parse_block_statement()?);
        let mut alternative = None;

        if self.expect_peek(&Token::ELSE) {
            if !self.expect_peek(&left_brace) {
                return self.peek_error_expr(&left_brace);
            }

            alternative = Some(Box::new(self.parse_block_statement()?));
        }

        Ok(Expression::If {
            token,
            condition,
            consequence,
            alternative,
        })
    }

    // Block Statement {Token::LBRACE, {statement_1, ...} }
    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let token = self.curr_token.clone();
        self.next_token(); // move past {
        let mut statements = Vec::new();

        loop {
            if matches!(self.curr_token, Token::EOF | Token::RBRACE) {
                break;
            }

            let p = self.parse_statement()?;
            statements.push(p);

            self.next_token();
        }

        Ok(Statement::Block { token, statements })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut arguments = vec![];

        // arguments are optional
        if self.expect_peek(&Token::RPAREN) {
            return Ok(arguments);
        }

        self.next_token(); // move to first argument: Identifier | FunctionLiteral
        let arg = self.parse_expression(OpsPrecedence::Lowest)?;
        arguments.push(arg);

        loop {
            if !self.peek_token_is(&Token::COMMA) {
                break;
            }
            self.next_token(); // move TO comma
            self.next_token(); // move PAST comma
            let arg = self.parse_expression(OpsPrecedence::Lowest)?;
            arguments.push(arg);
        }

        let right_paren = Token::RPAREN;
        if !self.expect_peek(&right_paren) {
            return Err(format!(
                "Expected next token to be {}, got {} instead",
                right_paren, self.peek_token
            ));
        }

        Ok(arguments)
    }

    fn parse_call_expression(&mut self, function: Box<Expression>) -> Result<Expression, String> {
        let token = self.curr_token.clone();
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call {
            token,
            function,
            arguments,
        })
    }
}
