use crate::ast::*;
use crate::object::*;

// const TRUE: &Object = &Object::Boolean{value: true};
// const FALSE: &Object = &Object::Boolean{value: false};

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Expr(Expression::IntegerLiteral{token:_, value}) => Ok(Object::Integer{value}),
        Node::Expr(Expression::Boolean{token:_, value}) => Ok(Object::Boolean{value}),
        Node::Stmt(Statement::Expression{token:_, expression}) => eval(Node::Expr(expression)),
        Node::Prog(Program { statements, errors:_ }) => eval_statements(statements),
        Node::Expr(Expression::Prefix{operator, right}) => {
            let right = eval(Node::Expr(*right))?;
            eval_prefix_expression(&operator.to_string(), right)
        },
        Node::Expr(Expression::Infix{operator, left, right}) => {
            let right = eval(Node::Expr(*right))?;
            let left = eval(Node::Expr(*left))?;
            eval_infix_expression(&operator.to_string(), left, right)
        },
        _ => Err(format!("eval failed for {}", node)),
    }
}

fn eval_statements(statements: Vec<Statement>) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(Node::Stmt(statement))?;
    }
    Ok(result)
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, String> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(format!("eval prefix expression failed for {}", operator)),
    }
}

fn eval_bang_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Boolean{value} => Ok(Object::Boolean{value: !value}),
        Object::Integer{value} => {
            match value {
                0 => Ok(Object::Boolean{value: true}),  // !0 == true
                _ => Ok(Object::Boolean{value: false}), // !n == false, n > 0
            }
        },
        Object::Null => Ok(Object::Boolean{value: true}),
        // _ => Err("right is not a Boolean object".to_string()),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Integer{value} => Ok(Object::Integer{value: -value}),
        _ => Err("right is not a an Integer object".to_string()),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, String> {
    match (&left, &right) {
        (Object::Integer{value: l}, Object::Integer{value: r}) => eval_integer_infix_expression(operator, *l, *r),
        _ => Err(format!("eval infix expression failed for left:{}, right:{}", left, right)),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Result<Object, String> {
    match operator {
        "+" => Ok(Object::Integer{value: left + right}),
        "-" => Ok(Object::Integer{value: left - right}),
        "*" => Ok(Object::Integer{value: left * right}),
        "/" => Ok(Object::Integer{value: left / right}),
        _ => Err(format!("eval integer infix expression faileds for left:{}, right:{}", left, right)),
    }
}
