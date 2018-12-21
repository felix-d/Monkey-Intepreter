use crate::ast::{BlockStatement, Expression, Program, Statement, Identifier};
use crate::environment::Environment;
use crate::object::{Object, Unwrap};
use std::rc::Rc;

macro_rules! check_err {
    ($expr:expr) => {
        if let Object::Error(_) = $expr {
            return $expr;
        }
    };
}

pub(crate) trait Eval {
    fn eval(self, env: Environment) -> Object;
}

impl Eval for Program {
    fn eval(self, env: Environment) -> Object {
        let mut result = Object::Null;

        for statement in self.statements {
            result = statement.eval(env.clone());

            match result {
                Object::ReturnValue(value) => return *value,
                Object::Error(_) => return result,
                _ => (),
            }
        }

        result
    }
}

impl Eval for BlockStatement {
    fn eval(self, env: Environment) -> Object {
        let mut result = Object::Null;

        for statement in self.0 {
            result = statement.eval(env.clone());

            match result {
                Object::ReturnValue(_) | Object::Error(_) => return result,
                _ => (),
            }
        }

        result
    }
}

impl Eval for Rc<BlockStatement> {
    fn eval(self, env: Environment) -> Object {
        let mut result = Object::Null;

        for statement in &self.0 {
            let statement = statement.clone();
            result = statement.eval(env.clone());

            match result {
                Object::ReturnValue(_) | Object::Error(_) => return result,
                _ => (),
            }
        }

        result
    }
}

impl Eval for Statement {
    fn eval(self, mut env: Environment) -> Object {
        match self {
            Statement::Return(expression) => {
                let value = expression.eval(env);
                check_err!(value);
                Object::new_return_value(value)
            }
            Statement::Expression(expression) => expression.eval(env),
            Statement::Let { name, value } => {
                let value = value.eval(env.clone());
                check_err!(value);
                env.set(name, value.clone());
                value
            }
            _ => unreachable!(),
        }
    }
}

impl Eval for Expression {
    fn eval(self, env: Environment) -> Object {
        match self {
            Expression::IntegerLiteral(integer) => Object::new_integer(integer),
            Expression::Prefix { operator, right } => {
                let right = right.eval(env);
                check_err!(right);
                eval_prefix_expression(operator, right)
            }
            Expression::Infix {
                left,
                right,
                operator,
            } => {
                let left = left.eval(env.clone());
                check_err!(left);

                let right = right.eval(env);
                check_err!(right);

                eval_infix_expression(operator, left, right)
            }
            Expression::Boolean(true) => Object::Boolean(true),
            Expression::Boolean(false) => Object::Boolean(false),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => eval_if_expression(condition, consequence, alternative, env),
            Expression::Identifier(identifier) => eval_identifier(identifier, env),
            Expression::FunctionLiteral { params, body } => {
                Object::new_function(Rc::new(params), Rc::new(body), env.clone())
            }
            Expression::CallExpression { function, args } => {
                let value = function.eval(env.clone());
                check_err!(value);

                let mut args = eval_expressions(args, env);

                if args.len() == 1 {
                    if let Object::Error(_) = args[0] {
                        return args.remove(0);
                    }
                }

                apply_function(value, args)
            }
            _ => unreachable!(),
        }
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function { body, params, env } => {
            let extended_env = extended_function_env(params, env, args);
            body.eval(extended_env).unwrap()
        }
        _ => Object::new_error(format!("not a function: {}", function.type_name())),
    }
}

fn extended_function_env(params: Rc<Vec<Identifier>>, env: Environment, args: Vec<Object>) -> Environment {
    let mut env = Environment::new_enclosed_environment(env.clone());
    for (i, param) in params.iter().enumerate() {
        env.set(param.clone(), args[i].clone());
    }
    env
}

fn eval_expressions(arguments: Vec<Expression>, env: Environment) -> Vec<Object> {
    let mut result = vec![];
    for arg in arguments {
        let value = arg.eval(env.clone());
        if let Object::Error(_) = value {
            return vec![value];
        }
        result.push(value);
    }
    result
}

fn eval_identifier(identifier: String, env: Environment) -> Object {
    match env.get(&identifier) {
        Some(val) => val,
        None => Object::new_error(format!("identifier not found: {}", identifier)),
    }
}

fn eval_if_expression(
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
    env: Environment,
) -> Object {
    let condition = condition.eval(env.clone());
    check_err!(condition);

    if is_truthy(condition) {
        consequence.eval(env)
    } else if let Some(alternative) = alternative {
        alternative.eval(env)
    } else {
        Object::Null
    }
}

fn is_truthy(val: Object) -> bool {
    match val {
        Object::Boolean(false) | Object::Null => false,
        _ => true,
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_ref() {
        "!" => eval_bang_expression(right),
        "-" => eval_minus_prefix_expression(right),
        _ => Object::new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.type_name()
        )),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    if left.is_integer() && right.is_integer() {
        return eval_infix_integer_expression(operator, left, right);
    }

    if left.is_bool() && right.is_bool() {
        return eval_infix_bool_expression(operator, left, right);
    }

    Object::new_error(format!(
        "type mismatch: {} {} {}",
        left.type_name(),
        operator,
        right.type_name()
    ))
}

fn eval_infix_integer_expression(operator: String, left: Object, right: Object) -> Object {
    let left_val: i64 = left.unwrap();
    let right_val: i64 = right.unwrap();

    match operator.as_ref() {
        "+" => Object::Integer(left_val + right_val),
        "-" => Object::Integer(left_val - right_val),
        "/" => Object::Integer(left_val / right_val),
        "*" => Object::Integer(left_val * right_val),
        "<" => Object::Boolean(left_val < right_val),
        ">" => Object::Boolean(left_val > right_val),
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        _ => Object::new_error(format!(
            "unknown operator: {} {} {}",
            left.type_name(),
            operator,
            right.type_name(),
        )),
    }
}

fn eval_infix_bool_expression(operator: String, left: Object, right: Object) -> Object {
    let left_val: bool = left.unwrap();
    let right_val: bool = right.unwrap();

    match operator.as_ref() {
        "==" => Object::Boolean(left_val == right_val),
        "!=" => Object::Boolean(left_val != right_val),
        _ => Object::new_error(format!(
            "unknown operator: {} {} {}",
            left.type_name(),
            operator,
            right.type_name(),
        )),
    }
}

fn eval_bang_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::new_error(format!("unknown operator: -{}", right.type_name())),
    }
}

#[cfg(test)]
mod tests {
    use super::Eval;
    use crate::environment::Environment;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();
        program.eval(env)
    }

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Integer(value) => assert_eq!(expected, value),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let cases = vec![
            ("true", true),
            ("false", false),
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Boolean(actual) => assert_eq!(expected, actual),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Boolean(actual) => assert_eq!(expected, actual),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_eval_if_else_expression() {
        let cases = vec![
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Integer(actual) => assert_eq!(expected, Some(actual)),
                Object::Null => assert_eq!(expected, None),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let cases = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r"
                  if (10 > 1) {
                  if (10 > 1) {
                    return 10;
                  }

                  return 1;
                }
            ",
                10,
            ),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Integer(actual) => assert_eq!(expected, actual),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r"
                  if (10 > 1) {
                    if (10 > 1) {
                      return true + false;
                    }

                    return 1;
                  }
             ",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Error(actual) => assert_eq!(expected, *actual),
                _ => unreachable!("Got {:?}, but expected Object::Error.", object),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let cases = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Integer(actual) => assert_eq!(expected, actual),
                _ => unreachable!("Got {:?}, but expected Object::Integer.", object),
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = r"fn(x) { x + 2; };";
        let object = test_eval(input.to_string());
        match object {
            Object::Function { params, body, .. } => {
                assert_eq!(1, params.len());
                assert_eq!("x", params[0]);
                assert_eq!("(x + 2)", format!("{}", body));
            }
            _ => unreachable!("Got {:?}, but expected Object::Function."),
        }
    }

    #[test]
    fn test_function_application() {
        let cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in cases {
            let object = test_eval(input.to_string());
            match object {
                Object::Integer(actual) => assert_eq!(expected, actual),
                _ => unreachable!("Got {:?}, but expected Object::Integer.", object),
            }
        }
    }
}
