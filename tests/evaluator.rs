extern crate monkey;

use std::vec;

use monkey::evaluator::*;
use monkey::lexer::*;
use monkey::object::*;
use monkey::parser::*;

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests.iter() {
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, *expected);
    }
}

fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input.chars().collect());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    let mut env = Environment::new();
    let evaluation = eval(program, &mut env);
    match evaluation {
        Ok(obj) => obj,
        _ => Object::Null,
    }
}

fn test_integer_object(obj: &Object, expected: i64) {
    match obj {
        Object::Integer { value } => {
            assert_eq!(*value, expected);
        }
        _ => assert!(false, "object is not Integer. got {}", obj.get_type()),
    }
}

fn test_boolean_object(obj: &Object, expected: bool) {
    match obj {
        Object::Boolean { value } => {
            assert_eq!(*value, expected);
        }
        _ => assert!(false, "object is not Boolean. got {}", obj.get_type()),
    }
}

#[test]
fn test_eval_bool_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in tests.iter() {
        let evaluated = test_eval(input);
        test_boolean_object(&evaluated, *expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
        ("!!0", false),
    ];

    for (input, expected) in tests.iter() {
        let evaluated = test_eval(input);
        test_boolean_object(&evaluated, *expected);
    }
}

#[test]
fn test_if_else_expression() {
    let tests = vec![
        ("if (true) { 10 }", "10"),
        ("if (false) { 10 }", "null"),
        ("if (1) { 10 }", "10"),
        ("if (1 < 2) { 10 }", "10"),
        ("if (1 > 2) { 10 }", "null"),
        ("if (1 > 2) { 10 } else { 20 }", "20"),
        ("if (1 < 2) { 10 } else { 20 }", "10"),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match i64::from_str_radix(expected, 10) {
            Ok(n) => test_integer_object(&evaluated, n),
            _ => test_null_object(&evaluated),
        }
    }
}

fn test_null_object(obj: &Object) {
    match obj {
        Object::Null => (),
        _ => assert!(false, "object is not NULL, got {}", obj),
    }
}

#[test]
fn test_return_statement() {
    let tests = vec![
        ("return 10;", "10"),
        ("return 10; 9;", "10"),
        ("return 2 * 5; 9;", "10"),
        ("9; return 2 * 5; 9;", "10"),
        (
            "if (10 > 1) { 
            if (10 > 1) {
                return 10; 
            }
            return 1; 
        }",
            "10",
        ),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match i64::from_str_radix(expected, 10) {
            Ok(n) => test_integer_object(&evaluated, n),
            _ => assert!(false, "type not expected. got {}", expected),
        }
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "true + false + true + false;",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "
        if (10 > 1) {
            if (10 > 1) {
                return true + false;
            }
        
            return 1;
        }
        ",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        ("foobar", "identifier not found: foobar"),
    ];

    for (input, expected) in tests {
        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        let evaluation = eval(program, &mut env);
        match evaluation {
            Ok(_obj) => assert!(false, "expected error not found!"),
            Err(msg) => {
                assert_eq!(expected, &msg);
            }
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(&evaluated, expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; }";
    let evaluated = test_eval(input);
    match evaluated {
        Object::Function {
            parameters,
            body,
            env: _,
        } => {
            assert_eq!(parameters.len(), 1);
            assert_eq!(parameters[0].to_string(), "x".to_string());
            let expected_body = "(x + 2)".to_string();
            assert_eq!(body.to_string(), expected_body);
        }
        _ => assert!(false, "object is not a Function, got {:?}", evaluated),
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for (input, expected) in tests {
        test_integer_object(&test_eval(input), expected);
    }
}

#[test]
fn test_closures() {
    let tests = vec![(
        "
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);",
        4,
    )];

    for (input, expected) in tests {
        test_integer_object(&test_eval(input), expected);
    }
}
