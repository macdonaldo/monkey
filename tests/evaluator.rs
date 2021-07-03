extern crate monkey;

use std::vec;

use monkey::lexer::*;
use monkey::parser::*;
use monkey::ast::*;
use monkey::object::*;
use monkey::evaluator::*;

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
    let program = Node::Prog(p.parse_program());
    let evaluation = eval(program);
    match evaluation {
        Ok(obj) => obj,
        _ => Object::Null, 
    }
}

fn test_integer_object(obj: &Object, expected: i64) {
    match obj {
        Object::Integer { value } => {
            assert_eq!(*value, expected);
        },
        _ => assert!(false, "object is not Integer. got {}", obj.get_type()),
    }
}

fn test_boolean_object(obj: &Object, expected: bool) {
    match obj {
        Object::Boolean { value } => {
            assert_eq!(*value, expected);
        },
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
