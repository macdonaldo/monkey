extern crate monkey;

use std::vec;

use monkey::lexer::*;
use monkey::token::*;
use monkey::parser::*;
use monkey::ast::*;

#[test]
fn let_statements_1() {
    let input = "
let x = 5;
let y = 10;
let  foobar = 838383;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    
    assert!(!program.statements.is_empty(), 
            "parse_program failed to pass any statement");
    assert!(program.statements.len() == 3, 
            "program.statements does not contain 3 statements. Got {}", program.statements.len());
    let expected_identifier = vec![
        Token::IDENT(String::from("x")),
        Token::IDENT(String::from("y")),
        Token::IDENT(String::from("foobar")),
    ];

    for (expect_tok, actual_stmt) in expected_identifier.iter().zip(program.statements.iter()) {
        match actual_stmt {
            Statement::Let{token, identifier, value:_} => {
                assert!(*token == Token::LET, 
                                                "Token literal not 'let'. got {:?}", *token);
                assert!(*identifier == *expect_tok,
                                                "Token.IDENT not {:?}. got {:?}", *expect_tok, *token);
            },
            _ => assert!(true, "stmt not LetStatement. got {:?} instead", actual_stmt),
        };
    }

    check_errors(&program.errors);
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let x = 5;", "x", "5"),
        ("let y = true;", "y", "true"),
        ("let foobar = y;", "foobar", "y"),
    ];

    for (input, expected_ident, expected_value) in tests.iter() {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(&program.errors);
    
        assert_eq!(program.statements.len(), 1);
        let expr_stmt = &program.statements[0];

        test_let_statement(expr_stmt, expected_ident, expected_value);
    }
}

fn test_let_statement(statement: &Statement, expected_ident: &str, expected_value: &str) {
    match statement {
        Statement::Let{token, identifier, value} => {
            assert!(*token == Token::LET, "Token literal not 'let'. got {:?}", *token);
            test_literal_expression(value, expression_from_literal(expected_value));

            match identifier {
                Token::IDENT(actual) => assert_eq!(*actual, expected_ident.to_string()),
                _ => assert!(false, "identifier not Token::IDENT(..), got {}", identifier),
            }
        },
        _ => assert!(true, "stmt not LetStatement. got {} instead", statement),
    };
}

fn check_errors(errors: &Vec<String>) {
    if errors.is_empty() {
        return;
    }

    println!("Parser has {} errors", errors.len());
    for e in errors.iter() {
        println!("{}", e);
    }

    assert!(false, "The above errors were found in parser.errors!");
}

#[test]
fn return_statements_1() {
    let input = "
return 5;
return 10;
return 992211;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    
    assert!(!program.statements.is_empty(), 
            "parse_program failed to pass any statement");
    assert!(program.statements.len() == 3, 
            "program.statements does not contain 3 statements. Got {}", program.statements.len());

    for actual_stmt in program.statements.iter() {
        match actual_stmt {
            Statement::Return{token, return_value:_} => {
                assert!(*token == Token::RETURN, 
                                                "Token literal not 'return'. got {:?}", *token);
            },
            _ => assert!(true, "stmt not ReturnStatement. got {:?} instead", actual_stmt),
        };
    }

    check_errors(&program.errors);
}

#[test]
fn pretty_printing() {
    let input = "
        let    x   =     y;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();

    assert_eq!("let x = y;", format!("{}", program));
}

#[test]
fn identifer_expressions() {
    let input = "
        foobar;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let expr_stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = expr_stmt {
        assert_eq!(std::mem::discriminant(&Expression::Identifier(Token::ILLEGAL)),
            std::mem::discriminant(expression));

        // Check that identifier expression has the value `foobar`
        if let Expression::Identifier(Token::IDENT(x)) = &expression {
            assert_eq!(x, "foobar");
        }
    }
    else {
        assert!(false, "program.statements[0] is not an Expression Statement!");
    }
}

#[test]
fn test_integer_literal_expressions() {
    let input = "
        5;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let expr_stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = expr_stmt {
        test_integer_literal(expression, 5);
    }
    else {
        assert!(false, "program.statements[0] is not an Expression Statement!");
    }
}

fn test_integer_literal(il: &Expression, value: i64) {
    // Check: Expression is an integer literal
    // Check: IntegerLiteral(Token) is Token::INT(_)
    // Check: IntegerLiteral(i64) is value
    match il {
        Expression::IntegerLiteral{token, value: val} => {
            match token {
                Token::INT(_) => {
                    assert_eq!(val, &value);
                },
                _ => assert!(false, "expected Token::INT(_), instead found {}", token),
            };
        },
        _ => assert!(false, "expected Expression::IntegerLiteral, instead found {}", il),
    };
}

fn test_identifier(id_expr: &Expression, value: &str) {
    // Check: Expression is Identifier Expression
    // Check: Identifier(Token), Token is Token::Identifier(Value)
    // Check: Value is value
    match id_expr {
        Expression::Identifier(token) => {
            match token {
                Token::IDENT(val) => {
                    assert_eq!(&val, &value);
                },
                _ => assert!(false, "expected Token::Identifier(_), instead found {}", token),
            };
        },
        _ => assert!(false, "expected Expression::Identifier, instead found {}", id_expr),
    };
}

fn test_literal_expression(expression: &Expression, expected: Expression) {
    match expected {
        Expression::IntegerLiteral{token:_, value} => test_integer_literal(expression, value),
        Expression::Identifier(Token::IDENT(value)) => test_identifier(expression, &value),
        Expression::Boolean{token:_, value} => test_boolean_literal(expression, value),
        _ => (),
    }
}

fn expression_from_literal(literal: &str) -> Expression {
    match i64::from_str_radix(literal, 10) {
        Ok(value) => Expression::IntegerLiteral{token: Token::INT(literal.to_string()), value},
        _ => {
            if literal == "true" {
                Expression::Boolean{token: Token::TRUE, value: true}
            }
            else if literal == "false" {
                Expression::Boolean{token: Token::FALSE, value: false}
            }
            else {
                Expression::Identifier(Token::IDENT(literal.to_string()))
            }
        },
    }
}

fn test_infix_expression(expr: &Expression, left_expr: &str, op: &str, right_expr: &str) {
    match expr {
        Expression::Infix{left, right, operator} => {
            assert_eq!(op, format!("{}", operator));

            let l = expression_from_literal(left_expr);
            test_literal_expression(left, l);

            let r = expression_from_literal(right_expr);
            test_literal_expression(right, r);
        },
        _ => assert!(false, "expected Expression::Infix, instead found {}", expr),
    }
}

#[test]
fn parsing_prefix_expressions() {
    let expected = vec![
        ("!5;", "!", "5"),
        ("-15;", "-", "15"),
    ];

    for (input, expctd_op, expctd_value) in &expected {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_errors(&program.errors);
    
        assert_eq!(program.statements.len(), 1);
        let expr_stmt = &program.statements[0];
        if let Statement::Expression{token:_, expression} = expr_stmt {
            match expression {
                Expression::Prefix{operator, right} => {
                    assert_eq!(format!("{}", operator), *expctd_op);
                    let expr = expression_from_literal(expctd_value);
                    test_literal_expression(right, expr);
                },
                _ => assert!(false, "statement is not Expression::Prefix"),
            };
        }
        else {
            assert!(false, "program.statements[0] is not an Expression Statement!");
        }
    }
}

#[test]
fn parsing_infix_expressions() {
    let expected = vec![
        ("5 + 5;", "5", "+", "5"),
        ("5 - 5;", "5", "-", "5"),
        ("5 * 5;", "5", "*", "5"),
        ("5 / 5;", "5", "/", "5"),
        ("5 > 5;", "5", ">", "5"),
        ("5 < 5;", "5", "<", "5"),
        ("5 == 5;", "5", "==", "5"),
        ("5 != 5;", "5", "!=", "5"),
        ("true == true", "true", "==", "true"),
        ("true != false", "true", "!=", "false"),
        ("false == false", "false", "==", "false"),
    ];

    for (input, left_value, op, right_value) in &expected {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(&program.errors);
    
        assert_eq!(program.statements.len(), 1);
        let expr_stmt = &program.statements[0];
        if let Statement::Expression{token:_, expression} = expr_stmt {
            test_infix_expression(expression, left_value, op, right_value);
        }
        else {
            assert!(false, "program.statements[0] is not an Expression Statement!");
        }
    }
}

#[test]
fn operator_precedence_parsing_expressions() {
    let expected = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
            (
            "a * b * c",
                    "((a * b) * c)",
                ),
            (
            "a * b / c",
                    "((a * b) / c)",
                ),
            (
            "a + b / c",
                    "(a + (b / c))",
                ),
                (
                    "a + b * c + d / e - f",
                    "(((a + (b * c)) + (d / e)) - f)",
            ), (
                    "3 + 4; -5 * 5",
                    "(3 + 4)((-5) * 5)",
                ),
                (
                    "5 > 4 == 3 < 4",
                    "((5 > 4) == (3 < 4))",
            ), (
                    "5 < 4 != 3 > 4",
                    "((5 < 4) != (3 > 4))",
                ),
                (
                    "3 + 4 * 5 == 3 * 1 + 4 * 5",
                    "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
            "true",
            "true", 
            ),
            (
            "false",
            "false", ),
            (
                "3 > 5 == false",
                "((3 > 5) == false)",
            ), 
            (
                "3 < 5 == true",
                "((3 < 5) == true)",
            ),
            (
                "1 + (2 + 3) + 4",
                "((1 + (2 + 3)) + 4)",
            ),
            (
                "(5 + 5) * 2",
                "((5 + 5) * 2)",
            ),
            (
                "2 / (5 + 5)",
                "(2 / (5 + 5))",
            ),
            (
                "(5 + 5) * 2 * (5 + 5)",
                "(((5 + 5) * 2) * (5 + 5))",
            ),
            (
                "-(5 + 5)",
                "(-(5 + 5))",
            ),
            (
                "!(true == true)",
                "(!(true == true))",
            ),
            (
                "a + add(b * c) + d",
                "((a + add((b * c))) + d)",
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
    ];

    for (input, expected_out) in &expected {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(&program.errors);
        assert_eq!(format!("{}", program), expected_out.to_string());
    }
}

fn test_boolean_literal(bool_expr: &Expression, val: bool) {
    // Check: Expression is Boolean Expression
    // Check: Bool{Token, value), Token is Token::(TRUE/FALSE)
    // Check: Value is value
    match bool_expr {
        Expression::Boolean{token, value} => {
            match token {
                Token::TRUE | Token::FALSE => {
                    assert_eq!(&val, value);
                },
                _ => assert!(false, "expected Token::TRUE / Token::FALSE, instead found {}", token),
            };
        },
        _ => assert!(false, "expected Expression::Boolean, instead found {}", bool_expr),
    };
}

#[test]
fn test_boolean_expressions() {
    let input = "
        true;
    ";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let expr_stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = expr_stmt {
        test_boolean_literal(expression, true);
    }
    else {
        assert!(false, "program.statements[0] is not an Expression Statement!");
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = stmt {
        match expression {
            Expression::If { token:_, condition, consequence, alternative } => {
                test_infix_expression(condition, "x", "<", "y");
                assert_eq!(None, *alternative);

                match consequence.as_ref() {
                    Statement::Block{token:_, statements} => {
                        assert_eq!(statements.len(), 1);
                        let s = &statements[0];
                        match s {
                            Statement::Expression{token:_, expression} => {
                                test_identifier(expression, "x");
                            },
                            _ => assert!(false, "statements[0] is not an Expression Statement"),
                        }
                    },
                    _ => assert!(false, "consequence is not a Block Statement, got {}", consequence),
                }
            }
            _ => assert!(false, "stmt.expression is not an If Expression!, got {}", stmt),

        }
    }
    else {
        assert!(false, "stmt is not an Expression Statement!");
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = stmt {
        match expression {
            Expression::If { token:_, condition, consequence, alternative } => {
                test_infix_expression(condition, "x", "<", "y");

                match consequence.as_ref() {
                    Statement::Block{token:_, statements} => {
                        assert_eq!(statements.len(), 1);
                        let s = &statements[0];
                        match s {
                            Statement::Expression{token:_, expression} => {
                                test_identifier(expression, "x");
                            },
                            _ => assert!(false, "consequence.statements[0] is not an Expression Statement"),
                        }
                    },
                    _ => assert!(false, "consequence is not a Block Statement, got {}", consequence),
                }

                match alternative {
                    Some(b) => {
                        match b.as_ref() {
                            Statement::Block{token:_, statements} => {
                                assert_eq!(statements.len(), 1);
                                let s = &statements[0];
                                match s {
                                    Statement::Expression{token:_, expression} => {
                                        test_identifier(expression, "y");
                                    },
                                    _ => assert!(false, "alternative.statements[0] is not an Expression Statement"),
                                }
                            },
                            _ => assert!(false, "alternative is not a Block Statement, got {}", b),
                        }
                    }
                    _ => assert!(false, "alternative is None"),
                }
            }
            _ => assert!(false, "stmt.expression is not an If Expression!, got {}", stmt),

        }
    }
    else {
        assert!(false, "stmt is not an Expression Statement!");
    }
}

#[test]
fn test_parse_function_literal() {
    let input = "fn(x, y) { x + y }";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let expr_stmt = &program.statements[0];
    if let Statement::Expression{token:_, expression} = expr_stmt {
        match expression {
            Expression::FunctionLiteral { token:_, parameters, body } => {
                test_literal_expression(&parameters[0], expression_from_literal("x"));
                test_literal_expression(&parameters[1], expression_from_literal("y"));

                match body.as_ref() {
                    Statement::Block{token:_, statements} => {
                        assert_eq!(statements.len(), 1);
                        let s = &statements[0];
                        match s {
                            Statement::Expression{token:_, expression} => {
                                test_infix_expression(expression, "x", "+", "y");
                            },
                            _ => assert!(false, "function body statement is not an Expression Statement, got {}", s),
                        }
                    },
                    _ => assert!(false, "body is not a Block Statement, got {}", body),
                }
            },
            _ => assert!(false, "expression is not a FunctionLiteral Expression, got {}", expression),

        }
    }
    else {
        assert!(false, "program.statements[0] is not an Expression Statement!");
    }
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for (input, expected_params) in tests.iter() {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_errors(&program.errors);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];

        if let Statement::Expression{token:_, expression} = stmt {
            match expression {
                Expression::FunctionLiteral{token:_, parameters, body:_}=> {
                    assert_eq!(parameters.len(), expected_params.len());
                    
                    for (expr, expected) in parameters.iter().zip(expected_params.iter()).next() {
                        test_literal_expression(expr, expression_from_literal(expected));
                    }
                }
                _ => assert!(false, "stmt.expression is not a FunctionLiteral, got {}", stmt),
    
            }
        }
        else {
            assert!(false, "stmt is not an Expression Statement!");
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_errors(&program.errors);

    assert_eq!(program.statements.len(), 1);
    let stmt = &program.statements[0];

    match stmt {
        Statement::Expression{token:_, expression} => {
            match expression {
                Expression::Call{ token:_, function, arguments } => {
                    test_identifier(function, "add");
                    assert_eq!(arguments.len(), 3);
                    test_literal_expression(&arguments[0], expression_from_literal("1"));
                    test_infix_expression(&arguments[1], "2", "*", "3");
                    test_infix_expression(&arguments[2], "4", "+", "5");
                },
                _ => assert!(false, "stmt.expression is not a Call Expression, got a {}", expression),
            }
        },
        _ => assert!(false, "stmt is not an Expression Statement, got {}", stmt),
    }
}

#[test]
fn test_call_expression_parameter_parsing() {
    let tests = vec![
        ("add();", "add", vec![]),
        ("add(1);", "add", vec!["1"]),
        ("add(1, 2 * 3, 4 + 5);", "add", vec!["1", "(2 * 3)", "(4 + 5)"]),
    ];

    for (input, expected_ident, expected_args) in tests.iter() {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_errors(&program.errors);
        
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        
        match stmt {
            Statement::Expression{token:_, expression} => {
                match expression {
                    Expression::Call{ token:_, function, arguments } => {
                        test_identifier(function, expected_ident);
                        assert_eq!(arguments.len(), expected_args.len());

                        for (actual, expected) in arguments.iter().zip(expected_args.iter()) {
                            assert_eq!(format!("{}", actual), expected.to_string());
                        }
                    },
                    _ => assert!(false, "stmt.expression is not a Call Expression, got a {}", expression),
                }
            },
            _ => assert!(false, "stmt is not an Expression Statement, got {}", stmt),
        }
    }
}

