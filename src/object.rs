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
    String {
        value: String,
    },
}

impl Object {
    pub fn get_type(&self) -> String {
        match self {
            Object::Integer { .. } => "INTEGER",
            Object::Boolean { .. } => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue { .. } => "RETURN_VALUE",
            Object::Function { .. } => "FUNCTION",
            Object::String { .. } => "STRING",
        }
        .to_string()
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
            Object::String { value } => write!(f, "{}", value),
        }
    }
}
