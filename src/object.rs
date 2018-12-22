use std::fmt;
use std::rc::Rc;
use crate::ast::{Identifier, BlockStatement};
use crate::environment::Environment;

#[derive(Debug)]
pub(crate) enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Rc<Object>),
    Error(String),
    Function {
        params: Vec<Identifier>,
        body: BlockStatement,
        env: Environment,
    },
    Null,
}

impl Object {
    pub(crate) fn new_integer(integer: i64) -> Rc<Self> {
        Rc::new(Object::Integer(integer))
    }

    pub(crate) fn new_null() -> Rc<Self> {
        Rc::new(Object::Null)
    }

    pub(crate) fn new_bool(value: bool) -> Rc<Self> {
        Rc::new(Object::Boolean(value))
    }

    pub(crate) fn new_return_value(value: Rc<Object>) -> Rc<Self> {
        Rc::new(Object::ReturnValue(value))
    }

    pub(crate) fn new_error(error: String) -> Rc<Self> {
        Rc::new(Object::Error(error))
    }

    pub(crate) fn new_function(params: Vec<Identifier>, body: BlockStatement, env: Environment) -> Rc<Self> {
        Rc::new(Object::Function {
            params,
            body,
            env,
        })
    }
}

pub trait Unwrap<T> {
    fn unwrap(&self) -> T;
}

impl Unwrap<i64> for Object {
    fn unwrap(&self) -> i64 {
        match self {
            Object::Integer(value) => *value,
            _ => panic!("{:?} cannot be unwrapped as i64.", self)
        }
    }
}

impl Unwrap<Rc<Object>> for Object {
    fn unwrap(&self) -> Rc<Object> {
        match self {
            Object::ReturnValue(value) => value.clone(),
            _ => panic!("{:?} cannot be unwrapped as Object.", self)
        }
    }
}

impl Unwrap<bool> for Object {
    fn unwrap(&self) -> bool {
        match self {
            Object::Boolean(value) => *value,
            _ => panic!("{:?} cannot be unwrapped as bool.", self)
        }
    }
}

impl Object {
    pub fn is_integer(&self) -> bool {
        match self {
            Object::Integer(_) => true,
            _ => false
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Object::Boolean(_) => true,
            _ => false
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::ReturnValue(_) => "RETURN",
            Object::Error(_) => "ERROR",
            Object::Function { .. } => "FUNCTION",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(object) => write!(f, "{}", object),
            Object::Error(error) => write!(f, "{}: {}", self.type_name(), error),
            Object::Function { params, body, .. } => {
                let params = params.join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, body)
            },
        }
    }
}
