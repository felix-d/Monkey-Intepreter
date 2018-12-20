use crate::ast::{Expression, Program, Statement, BlockStatement};
use crate::object::Object;

pub(crate) trait Eval {
    fn eval(self) -> Object;
}

impl Eval for Program {
    fn eval(self) -> Object {
        self.statements.eval()
    }
}

impl Eval for Vec<Statement> {
    fn eval(self) -> Object {
        for statement in self {
            return statement.eval();
        }
        Object::Null
    }
}

impl Eval for Statement {
    fn eval(self) -> Object {
        match self {
            Statement::Expression(expression) => expression.eval(),
            _ => unreachable!(),
        }
    }
}

impl Eval for Expression {
    fn eval(self) -> Object {
        match self {
            Expression::IntegerLiteral(integer) => Object::new_integer(integer),
            Expression::Prefix { operator, right } => {
                let right = right.eval();
                eval_prefix_expression(operator, right)
            }
            Expression::Infix {
                left,
                right,
                operator,
            } => {
                let left = left.eval();
                let right = right.eval();
                eval_infix_expression(operator, left, right)
            }
            Expression::Boolean(true) => Object::Boolean(true),
            Expression::Boolean(false) => Object::Boolean(false),
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => eval_if_expression(condition, consequence, alternative),
            _ => unreachable!(),
        }
    }

}

impl Eval for BlockStatement {
    fn eval(self) -> Object {
        self.0.eval()
    }
}

fn eval_if_expression(condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Object {
    let condition = condition.eval();

    if is_truthy(condition) {
        consequence.eval()
    } else if let Some(alternative) = alternative {
        alternative.eval()
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
        _ => unreachable!(),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match left {
        Object::Integer(left) => match right {
            Object::Integer(right) => eval_infix_integer_expression(operator, left, right),
            _ => panic!(),
        },
        Object::Boolean(left) => match right {
            Object::Boolean(right) => eval_infix_bool_expression(operator, left, right),
            _ => panic!(),
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
        _ => panic!(),
    }
}

fn eval_infix_bool_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_ref() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => panic!(),
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
    use super::Eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        program.eval()
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
}
