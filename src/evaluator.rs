use crate::ast::*;
use crate::object::*;

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Prog(Program { statements, errors:_ }) => eval_program(statements),
        Node::Stmt(statement) => match statement {
            Statement::Expression{token:_, expression} => eval(Node::Expr(expression)),
            Statement::Block{token:_, statements} => eval_block_statemen(statements),
            Statement::Return{token:_, return_value} => {
                let value = eval(Node::Expr(return_value))?;
                Ok(Object::ReturnValue{value: Box::new(value)})
            }
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

fn eval_program(statements: Vec<Statement>) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(Node::Stmt(statement))?;

        if let Object::ReturnValue{value} = result {
            return Ok(*value);
        }
    }
    Ok(result)
}

fn eval_block_statemen(statements: Vec<Statement>) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(Node::Stmt(statement))?;

        if let Object::ReturnValue{..} = result {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, String> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(format!("unknown operator: {}{}", operator, right.get_type())),
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
        _ => Err(format!("unknown operator: !{}", right.get_type())),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Integer{value} => Ok(Object::Integer{value: -value}),
        _ => Err(format!("unknown operator: -{}", right.get_type())),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Result<Object, String> {
    match (&left, &right) {
        (Object::Integer{value: l}, Object::Integer{value: r}) => eval_integer_infix_expression(operator, *l, *r),
        (Object::Boolean{value: l}, Object::Boolean {value: r}) => eval_boolean_infix_expression(operator, *l, *r),
        _ => {
            match left.get_type() != right.get_type() {
                true => Err(format!("type mismatch: {} {} {}", left.get_type(), operator, right.get_type())),
                false => Err(format!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type())),
            }
        }
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
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: bool, right: bool) -> Result<Object, String> {
    match operator {
        "==" => Ok(Object::Boolean{value: left == right}),
        "!=" => Ok(Object::Boolean{value: left != right}),
        _ => Err(format!("unknown operator: {} {} {}", "BOOLEAN", operator, "BOOLEAN")),
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
        _ => return Err(format!("expected If Expression, got {:?}", expression)),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean{value} => value,
        _ => true,
    }
}
