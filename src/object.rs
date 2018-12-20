use std::fmt;

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub(crate) fn new_integer(integer: i64) -> Self {
        Object::Integer(integer)
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}
