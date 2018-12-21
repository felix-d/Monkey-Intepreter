use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::Object;

#[derive(Debug)]
pub(crate) struct Environment(Rc<RefCell<HashMap<String, Object>>>);

impl Clone for Environment {
    fn clone(&self) -> Self {
        Environment(self.0.clone())
    }
}

impl Environment {
    pub(crate) fn new() -> Self {
        Environment(Rc::new(RefCell::new(HashMap::new())))
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        self.0.borrow().get(key).map(Clone::clone)
    }

    pub fn set(&mut self, key: String, val: Object) {
        self.0.borrow_mut().insert(key, val);
    }
}
