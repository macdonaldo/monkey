use crate::ast::*;
use crate::object::*;

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Prog(Program { statements, errors:_ }) => eval_statements(statements),
        Node::Stmt(statement) => match statement {
            Statement::Expression{token:_, expression} => eval(Node::Expr(expression)),
            Statement::Block{token:_, statements} => eval_statements(statements),
            _ => Err(format!("eval failed for {}", statement)),
        }
        Node::Expr(expression) => {
            match expression {
                Expression::If{..} => eval_if_expression(expression),
                Expression::IntegerLiteral{token:_, value} => Ok(Object::Integer{value}),
                Expression::Boolean{token:_, value} => Ok(Object::Boolean{value}),
                Expression::Prefix{operator, right} => {
                    let right = eval(Node::Expr(*right))?;
                    eval_prefix_expression(&operator.to_string(), right)
                },
                Expression::Infix{operator, left, right} => {
                    let right = eval(Node::Expr(*right))?;
                    let left = eval(Node::Expr(*left))?;
                    eval_infix_expression(&operator.to_string(), left, right)
                },
                _ => Err(format!("eval failed for {}", expression)),
            }
        }
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
        (Object::Boolean{value: l}, Object::Boolean {value: r}) => eval_boolean_infix_expression(operator, *l, *r),
        _ => Err(format!("eval infix expression failed for left:{}, right:{}", left, right)),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Result<Object, String> {
    match operator {
        "+" => Ok(Object::Integer{value: left + right}),
        "-" => Ok(Object::Integer{value: left - right}),
        "*" => Ok(Object::Integer{value: left * right}),
        "/" => Ok(Object::Integer{value: left / right}),
        ">" => Ok(Object::Boolean{value: left > right}),
        "<" => Ok(Object::Boolean{value: left < right}),
        "==" => Ok(Object::Boolean{value: left == right}),
        "!=" => Ok(Object::Boolean{value: left != right}),
        _ => Err(format!("eval integer infix expression faileds for left:{}, right:{}", left, right)),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: bool, right: bool) -> Result<Object, String> {
    match operator {
        "==" => Ok(Object::Boolean{value: left == right}),
        "!=" => Ok(Object::Boolean{value: left != right}),
        _ => Err(format!("eval boolean infix expression faileds for left:{}, right:{}", left, right)),
    }
}

fn eval_if_expression(expression: Expression) -> Result<Object, String> {
    match expression {
        Expression::If{token:_, condition, consequence, alternative} => {
            let condition = eval(Node::Expr(*condition))?;
            if is_truthy(condition) {
                return eval(Node::Stmt(*consequence));
            }

            if let Some(alt) = alternative {
                return eval(Node::Stmt(*alt));
            }
            Ok(Object::Null)
        },
        _ => return Err("expression is not If expression".to_string()),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean{value} => value,
        _ => true,
    }
}
