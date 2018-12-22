use std::fmt;
use std::rc::Rc;
use crate::ast::{Identifier, BlockStatement};
use crate::environment::Environment;
use std::ops::Deref;

#[derive(Debug)]
pub(crate) struct Object(Rc<ObjectType>);

#[derive(Debug)]
pub(crate) enum ObjectType {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Object),
    Error(String),
    Function {
        params: Vec<Identifier>,
        body: BlockStatement,
        env: Environment,
    },
    Null,
}

impl Object {
    pub(crate) fn new_integer(integer: i64) -> Self {
        Object(Rc::new(ObjectType::Integer(integer)))
    }

    pub(crate) fn new_null() -> Self {
        Object(Rc::new(ObjectType::Null))
    }

    pub(crate) fn new_bool(value: bool) -> Self {
        Object(Rc::new(ObjectType::Boolean(value)))
    }

    pub(crate) fn new_return_value(value: Object) -> Self {
        Object(Rc::new(ObjectType::ReturnValue(value)))
    }

    pub(crate) fn new_error(error: String) -> Self {
        Object(Rc::new(ObjectType::Error(error)))
    }

    pub(crate) fn new_function(params: Vec<Identifier>, body: BlockStatement, env: Environment) -> Self {
        Object(Rc::new(ObjectType::Function {
            params,
            body,
            env,
        }))
    }
}

impl Deref for Object {
    type Target = ObjectType;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        Object(self.0.clone())
    }
}

pub trait Unwrap<T> {
    fn unwrap(&self) -> T;
}

impl Unwrap<i64> for Object {
    fn unwrap(&self) -> i64 {
        match **self {
            ObjectType::Integer(value) => value,
            _ => panic!("{:?} cannot be unwrapped as i64.", self)
        }
    }
}

impl Unwrap<Object> for Object {
    fn unwrap(&self) -> Object {
        match **self {
            ObjectType::ReturnValue(ref value) => value.clone(),
            _ => panic!("{:?} cannot be unwrapped as Object.", self)
        }
    }
}

impl Unwrap<bool> for Object {
    fn unwrap(&self) -> bool {
        match **self {
            ObjectType::Boolean(value) => value,
            _ => panic!("{:?} cannot be unwrapped as bool.", self)
        }
    }
}

impl Object {
    pub fn is_integer(&self) -> bool {
        match **self {
            ObjectType::Integer(_) => true,
            _ => false
        }
    }

    pub fn is_bool(&self) -> bool {
        match **self {
            ObjectType::Boolean(_) => true,
            _ => false
        }
    }

    pub fn type_name(&self) -> &str {
        match **self {
            ObjectType::Integer(_) => "INTEGER",
            ObjectType::Boolean(_) => "BOOLEAN",
            ObjectType::Null => "NULL",
            ObjectType::ReturnValue(_) => "RETURN",
            ObjectType::Error(_) => "ERROR",
            ObjectType::Function { .. } => "FUNCTION",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match **self {
            ObjectType::Integer(value) => write!(f, "{}", value),
            ObjectType::Boolean(value) => write!(f, "{}", value),
            ObjectType::Null => write!(f, "null"),
            ObjectType::ReturnValue(ref object) => write!(f, "{}", object),
            ObjectType::Error(ref error) => write!(f, "{}: {}", self.type_name(), error),
            ObjectType::Function { ref params, ref body, .. } => {
                let params = params.join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, body)
            },
        }
    }
}
