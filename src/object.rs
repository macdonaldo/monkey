use crate::ast::*;
use crate::evaluator::*;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Object {
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    Null,
    ReturnValue {
        value: Box<Object>,
    },
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Environment,
    },
}

impl Object {
    pub fn get_type(&self) -> String {
        let t = match self {
            Object::Integer { .. } => "INTEGER",
            Object::Boolean { .. } => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue { .. } => "RETURN_VALUE",
            Object::Function { .. } => "FUNCTION",
        };
        t.to_string()
    }

    pub fn inspect(&self) -> String {
        format!("{}", self)
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer { value } => write!(f, "{}", value),
            Object::Boolean { value } => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::ReturnValue { value } => write!(f, "{}", value.inspect()),
            Object::Function {
                parameters,
                body,
                env: _,
            } => {
                let mut parameter_list = String::new();
                for (i, e) in parameters.iter().enumerate() {
                    match i == 0 {
                        true => parameter_list.push_str(&format!("{}", e)),
                        _ => parameter_list.push_str(&format!(", {}", e)),
                    };
                }
                write!(f, "fn({}) {{{}}}", parameter_list, *body)
            }
        }
    }
}
