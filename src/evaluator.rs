use std::collections::HashMap;

use crate::ast::*;
use crate::object::*;

pub trait Evaluation {
    fn evaluate(self, env: &mut Environment) -> Result<Object, String>;
}

impl Evaluation for Program {
    fn evaluate(self, env: &mut Environment) -> Result<Object, String> {
        match self {
            Program {
                statements,
                errors: _,
            } => eval_program(statements, env),
        }
    }
}

impl Evaluation for Statement {
    fn evaluate(self, env: &mut Environment) -> Result<Object, String> {
        match self {
            Statement::Expression {
                token: _,
                expression,
            } => eval(expression, env),
            Statement::Block {
                token: _,
                statements,
            } => eval_block_statement(statements, env),
            Statement::Return {
                token: _,
                return_value,
            } => {
                let value = eval(return_value, env)?;
                Ok(Object::ReturnValue {
                    value: Box::new(value),
                })
            }
            Statement::Let {
                token: _,
                identifier,
                value,
            } => {
                let value = eval(value, env)?;
                env.set(identifier.to_string(), value);
                Ok(Object::Null)
            }
        }
    }
}

impl Evaluation for Expression {
    fn evaluate(self, env: &mut Environment) -> Result<Object, String> {
        match self {
            Expression::If { .. } => eval_if_expression(self, env),
            Expression::IntegerLiteral { token: _, value } => Ok(Object::Integer { value }),
            Expression::Boolean { token: _, value } => Ok(Object::Boolean { value }),
            Expression::Prefix { operator, right } => {
                let right = eval(*right, env)?;
                eval_prefix_expression(&operator.to_string(), right)
            }
            Expression::Infix {
                operator,
                left,
                right,
            } => {
                let right = eval(*right, env)?;
                let left = eval(*left, env)?;
                let operator = operator.to_string();
                eval_infix_expression(operator, left, right)
            }
            Expression::Identifier(token) => eval_identifier(&token.to_string(), env),
            Expression::FunctionLiteral {
                token: _,
                parameters,
                body,
            } => Ok(Object::Function {
                parameters,
                body: *body,
                env: env.clone(),
            }),
            Expression::Call {
                token: _,
                function,
                arguments,
            } => {
                let function = eval(*function, env)?;
                let args = eval_expressions(arguments, env)?;
                apply_function(function, args)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, identifier: &str) -> Option<&Object> {
        match self.store.contains_key(identifier) {
            true => self.store.get(identifier),
            false => match &self.outer {
                Some(out) => (*out).get(identifier),
                _ => None,
            },
        }
    }

    pub fn set(&mut self, identifier: String, value: Object) {
        self.store.insert(identifier, value);
    }

    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: &Environment) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer.clone())),
        }
    }
}

pub fn eval<T: Evaluation>(node: T, env: &mut Environment) -> Result<Object, String> {
    node.evaluate(env)
}

fn apply_function(function: Object, args: Vec<Object>) -> Result<Object, String> {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            let mut extended_env = extend_function_env(&env, parameters, args);
            let evaluated = eval(body, &mut extended_env)?;
            unwrap_return_value(evaluated)
        }
        _ => Err(format!("not a function: {}", function.get_type())),
    }
}

fn extend_function_env(
    current_env: &Environment,
    parameters: Vec<Expression>,
    args: Vec<Object>,
) -> Environment {
    let mut extend_env = Environment::new_enclosed(current_env);
    assert!(
        args.len() == parameters.len(),
        "argument count does not match parameter count. {} parameters, {} arguments",
        parameters.len(),
        args.len()
    );

    for (identifier, arg) in parameters.into_iter().zip(args.into_iter()) {
        extend_env.set(identifier.to_string(), arg);
    }
    extend_env
}

fn unwrap_return_value(obj: Object) -> Result<Object, String> {
    match obj {
        Object::ReturnValue { value } => Ok(*value),
        _ => Ok(obj),
    }
}

fn eval_expressions(exprs: Vec<Expression>, env: &mut Environment) -> Result<Vec<Object>, String> {
    let mut result = vec![];

    for expr in exprs {
        let evaluated = eval(expr, env)?;
        result.push(evaluated);
    }

    Ok(result)
}

fn eval_identifier(identifier: &str, env: &mut Environment) -> Result<Object, String> {
    let value = env.get(identifier);
    match value {
        Some(obj) => Ok(obj.clone()),
        _ => Err(format!("identifier not found: {}", identifier)),
    }
}

fn eval_program(statements: Vec<Statement>, env: &mut Environment) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(statement, env)?;

        if let Object::ReturnValue { value } = result {
            return Ok(*value);
        }
    }
    Ok(result)
}

fn eval_block_statement(
    statements: Vec<Statement>,
    env: &mut Environment,
) -> Result<Object, String> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval(statement, env)?;

        if let Object::ReturnValue { .. } = result {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_prefix_expression(operator: &str, right: Object) -> Result<Object, String> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(format!(
            "unknown operator: {}{}",
            operator,
            right.get_type()
        )),
    }
}

fn eval_bang_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Boolean { value } => Ok(Object::Boolean { value: !value }),
        Object::Integer { value } => {
            match value {
                0 => Ok(Object::Boolean { value: true }),  // !0 == true
                _ => Ok(Object::Boolean { value: false }), // !n == false, n > 0
            }
        }
        Object::Null => Ok(Object::Boolean { value: true }),
        _ => Err(format!("unknown operator: !{}", right.get_type())),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Result<Object, String> {
    match right {
        Object::Integer { value } => Ok(Object::Integer { value: -value }),
        _ => Err(format!("unknown operator: -{}", right.get_type())),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Result<Object, String> {
    match (&left, &right) {
        (Object::Integer { value: l }, Object::Integer { value: r }) => {
            eval_integer_infix_expression(operator, *l, *r)
        }
        (Object::Boolean { value: l }, Object::Boolean { value: r }) => {
            eval_boolean_infix_expression(operator, *l, *r)
        }
        _ => match left.get_type() != right.get_type() {
            true => Err(format!(
                "type mismatch: {} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            )),
            false => Err(format!(
                "unknown operator: {} {} {}",
                left.get_type(),
                operator,
                right.get_type()
            )),
        },
    }
}

fn eval_integer_infix_expression(
    operator: String,
    left: i64,
    right: i64,
) -> Result<Object, String> {
    match operator.as_str() {
        "+" => Ok(Object::Integer {
            value: left + right,
        }),
        "-" => Ok(Object::Integer {
            value: left - right,
        }),
        "*" => Ok(Object::Integer {
            value: left * right,
        }),
        "/" => Ok(Object::Integer {
            value: left / right,
        }),
        ">" => Ok(Object::Boolean {
            value: left > right,
        }),
        "<" => Ok(Object::Boolean {
            value: left < right,
        }),
        "==" => Ok(Object::Boolean {
            value: left == right,
        }),
        "!=" => Ok(Object::Boolean {
            value: left != right,
        }),
        _ => Err(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_boolean_infix_expression(
    operator: String,
    left: bool,
    right: bool,
) -> Result<Object, String> {
    match operator.as_str() {
        "==" => Ok(Object::Boolean {
            value: left == right,
        }),
        "!=" => Ok(Object::Boolean {
            value: left != right,
        }),
        _ => Err(format!(
            "unknown operator: {} {} {}",
            "BOOLEAN", operator, "BOOLEAN"
        )),
    }
}

fn eval_if_expression(expression: Expression, env: &mut Environment) -> Result<Object, String> {
    match expression {
        Expression::If {
            token: _,
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval(*condition, env)?;
            if is_truthy(condition) {
                return eval(*consequence, env);
            }

            if let Some(alt) = alternative {
                return eval(*alt, env);
            }
            Ok(Object::Null)
        }
        _ => return Err(format!("expected If Expression, got {:?}", expression)),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean { value } => value,
        _ => true,
    }
}
