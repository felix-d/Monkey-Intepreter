use crate::ast::{Node, Expression, Statement, Program};
use crate::object::{Object};

pub fn eval_program(program: Program) -> Object {
    for statement in program.statements {
        return eval_statement(statement);
    }
    Object::Null
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => unreachable!()
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::new_integer(integer),
        Expression::Prefix { operator, right } => {
            let right = eval_expression(*right);
            eval_prefix_expression(operator, right)
        },
        Expression::Infix { left, right, operator } => {
            let left = eval_expression(*left);
            let right = eval_expression(*right);
            eval_infix_expression(operator, left, right)
        }
        Expression::Boolean(true) => Object::Boolean(true),
        Expression::Boolean(false) => Object::Boolean(false),
        _ => unreachable!()
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_ref() {
        "!" => eval_bang_expression(right),
        "-" => eval_minus_prefix_expression(right),
        _ => unreachable!()
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match left {
        Object::Integer(left) => {
            match right {
                Object::Integer(right) => eval_infix_integer_expression(operator, left, right),
                _ => panic!(),
            }
        },
        Object::Boolean(left) => {
            match right {
                Object::Boolean(right) => eval_infix_bool_expression(operator, left, right),
                _ => panic!()
            }
        },
        _ => panic!(),
    }
}

fn eval_infix_integer_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_ref() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "/" => Object::Integer(left / right),
        "*" => Object::Integer(left * right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => panic!()
    }

}

fn eval_infix_bool_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_ref() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => panic!()
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
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::object::Object;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::eval_program;

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        eval_program(program)
    }

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            ("5".to_string(), 5),
            ("10".to_string(), 10),
            ("-5".to_string(), -5),
            ("-10".to_string(), -10),
            ("5 + 5 + 5 + 5 - 10".to_string(), 10),
            ("2 * 2 * 2 * 2 * 2".to_string(), 32),
            ("-50 + 100 + -50".to_string(), 0),
            ("5 * 2 + 10".to_string(), 20),
            ("5 + 2 * 10".to_string(), 25),
            ("20 + 2 * -10".to_string(), 0),
            ("50 / 2 * 2 + 10".to_string(), 60),
            ("2 * (5 + 10)".to_string(), 30),
            ("3 * 3 * 3 + 10".to_string(), 37),
            ("3 * (3 * 3) + 10".to_string(), 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(), 50),
        ];

        for (input, expected) in cases {
            let object = test_eval(input);
            match object {
                Object::Integer(value) => assert_eq!(expected, value),
                _ => unreachable!()
            }
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let cases = vec![
            ("true".to_string(), true),
            ("false".to_string(), false),
            ("true".to_string(), true),
            ("false".to_string(), false),
            ("1 < 2".to_string(), true),
            ("1 > 2".to_string(), false),
            ("1 < 1".to_string(), false),
            ("1 > 1".to_string(), false),
            ("1 == 1".to_string(), true),
            ("1 != 1".to_string(), false),
            ("1 == 2".to_string(), false),
            ("1 != 2".to_string(), true),
            ("true == true".to_string(), true),
            ("false == false".to_string(), true),
            ("true == false".to_string(), false),
            ("true != false".to_string(), true),
            ("false != true".to_string(), true),
            ("(1 < 2) == true".to_string(), true),
            ("(1 < 2) == false".to_string(), false),
            ("(1 > 2) == true".to_string(), false),
            ("(1 > 2) == false".to_string(), true),
        ];

        for (input, expected) in cases {
            let object = test_eval(input);
            match object {
                Object::Boolean(actual) => assert_eq!(expected, actual),
                _ => unreachable!()
            }
        }
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            ("!true".to_string(), false),
            ("!false".to_string(), true),
            ("!5".to_string(), false),
            ("!!true".to_string(), true),
            ("!!false".to_string(), false),
            ("!!5".to_string(), true),
        ];

        for (input, expected) in cases {
            let object = test_eval(input);
            match object {
                Object::Boolean(actual) => assert_eq!(expected, actual),
                _ => unreachable!()
            }
        }
    }
}
