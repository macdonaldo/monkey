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