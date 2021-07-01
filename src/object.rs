use std::fmt;

pub enum Object {
    Integer{value: i64},
    Boolean{value: bool},
    Null,
}

impl Object {
    pub fn get_type(&self) -> String {
        let t = match self {
            Object::Integer {..} => "INTEGER",
            Object::Boolean {..} => "BOOLEAN",
            Object::Null => "NULL",
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
            Object::Integer {value } => write!(f, "{}", value),
            Object::Boolean { value } => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}