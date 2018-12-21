use std::fmt;
use std::rc::Rc;
use crate::ast::{Identifier, BlockStatement};
use crate::environment::Environment;

#[derive(Debug)]
pub(crate) enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(Rc<String>),
    Function {
        params: Rc<Vec<Identifier>>,
        body: Rc<BlockStatement>,
        env: Environment,
    },
    Null,
}

impl Object {
    pub(crate) fn new_integer(integer: i64) -> Self {
        Object::Integer(integer)
    }

    pub(crate) fn new_return_value(value: Object) -> Self {
        Object::ReturnValue(Box::new(value))
    }

    pub(crate) fn new_error(error: String) -> Self {
        Object::Error(Rc::new(error))
    }

    pub(crate) fn new_function(params: Rc<Vec<Identifier>>, body: Rc<BlockStatement>, env: Environment) -> Self {
        Object::Function {
            params,
            body,
            env,
        }
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        match self {
            Object::Integer(value) => Object::Integer(*value),
            Object::Boolean(value) => Object::Boolean(*value),
            Object::Error(err) => Object::Error(err.clone()),
            Object::ReturnValue(value) => Object::ReturnValue(value.clone()),
            Object::Null => Object::Null,
            Object::Function { params, body, env } => Object::new_function(params.clone(), body.clone(), env.clone()),
        }
    }
}

pub trait Unwrap<T> {
    fn unwrap(&self) -> T;
}

impl Unwrap<i64> for Object {
    fn unwrap(&self) -> i64 {
        match *self {
            Object::Integer(value) => value,
            _ => panic!("{:?} cannot be unwrapped as i64.", self)
        }
    }
}

impl Unwrap<Object> for Object {
    fn unwrap(&self) -> Object {
        match self {
            Object::ReturnValue(value) => *value.clone(),
            _ => panic!("{:?} cannot be unwrapped as Object.", self)
        }
    }
}

impl Unwrap<bool> for Object {
    fn unwrap(&self) -> bool {
        match *self {
            Object::Boolean(value) => value,
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
            Object::Function { params, body, env } => {
                let params = params.join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, body)
            },
        }
    }
}
