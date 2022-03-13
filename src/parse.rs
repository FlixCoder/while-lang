use nom::{
	branch::alt,
	bytes::complete::{tag, tag_no_case},
	character::complete::{
		alpha0, alphanumeric1, anychar, char as nom_char, digit1, multispace0, multispace1,
	},
	combinator::{map, map_res, not, opt, recognize},
	error::Error,
	multi::{many0, separated_list0},
	sequence::{delimited, pair, tuple},
	Err,
};

use crate::language::*;

type MyIResult<'a, T> = nom::IResult<&'a str, T>;

fn nom_digit(input: &str) -> MyIResult<Digit> {
	map(map_res(digit1, |digit: &str| digit.parse()), Digit)(input)
}

fn nom_ident(input: &str) -> MyIResult<Ident> {
	map(recognize(pair(alpha0, alphanumeric1)), |ident: &str| Ident(ident.to_owned()))(input)
}

fn nom_fn_type(input: &str) -> MyIResult<FnType> {
	let (output, _ty) = tag_no_case("num")(input)?;
	Ok((output, FnType::Num))
}

fn nom_fn_call(input: &str) -> MyIResult<FnCall> {
	let (output, ident) = nom_ident(input)?;
	let (output, _) = multispace0(output)?;
	let white_spaced_expr = delimited(multispace0, nom_expression, multispace0);
	let (output, params) =
		delimited(nom_char('('), separated_list0(nom_char(','), white_spaced_expr), nom_char(')'))(
			output,
		)?;
	let fn_call = FnCall { name: ident, parameters: params };
	Ok((output, fn_call))
}

fn nom_expression(input: &str) -> MyIResult<Expression> {
	let expr_digit = map(nom_digit, Expression::Digit);
	let expr_ident = map(nom_ident, Expression::Variable);
	let expr_fn_call = map(nom_fn_call, Expression::FnCall);
	alt((expr_digit, expr_ident, expr_fn_call))(input)
}

fn nom_assignment(input: &str) -> MyIResult<Assignment> {
	let (output, name) = nom_ident(input)?;
	let (output, _) = tuple((multispace0, nom_char('='), multispace0))(output)?;
	let (output, expr) = nom_expression(output)?;
	let (output, _) = pair(multispace0, nom_char(';'))(output)?;
	let assignment = Assignment { variable: name, expression: expr };
	Ok((output, assignment))
}

fn nom_increment(input: &str) -> MyIResult<Increment> {
	let (output, name) = nom_ident(input)?;
	let (output, _) = tuple((multispace0, tag("+="), multispace0))(output)?;
	let (output, expr) = nom_expression(output)?;
	let (output, _) = pair(multispace0, nom_char(';'))(output)?;
	let increment = Increment { variable: name, expression: expr };
	Ok((output, increment))
}

fn nom_decrement(input: &str) -> MyIResult<Decrement> {
	let (output, name) = nom_ident(input)?;
	let (output, _) = tuple((multispace0, tag("-="), multispace0))(output)?;
	let (output, expr) = nom_expression(output)?;
	let (output, _) = pair(multispace0, nom_char(';'))(output)?;
	let decrement = Decrement { variable: name, expression: expr };
	Ok((output, decrement))
}

fn nom_while(input: &str) -> MyIResult<While> {
	let (output, _) = pair(tag_no_case("while"), multispace1)(input)?;
	let (output, variable) = nom_ident(output)?;
	let (output, _) = tuple((multispace0, tag("!="), multispace0))(output)?;
	let (output, digit) = nom_digit(output)?;
	let (output, _) = tuple((multispace0, nom_char(':'), multispace0))(output)?;
	let (output, inner_scope) = nom_scope(output)?;
	let (output, _) = pair(multispace0, nom_char('#'))(output)?;
	let r#while = While { variable, not: digit, scope: inner_scope };
	Ok((output, r#while))
}

fn nom_scope_item(input: &str) -> MyIResult<ScopeItem> {
	let item_assign = map(nom_assignment, ScopeItem::Assignment);
	let item_increment = map(nom_increment, ScopeItem::Increment);
	let item_decrement = map(nom_decrement, ScopeItem::Decrement);
	let item_fn = map(nom_fn_call, ScopeItem::FnCall);
	let item_while = map(nom_while, ScopeItem::While);
	alt((item_assign, item_increment, item_decrement, item_fn, item_while))(input)
}

fn nom_scope(input: &str) -> MyIResult<Scope> {
	let (output, items) = many0(delimited(multispace0, nom_scope_item, multispace0))(input)?;
	let scope = Scope { items };
	Ok((output, scope))
}

fn nom_function(input: &str) -> MyIResult<Function> {
	let (output, fn_type) = nom_fn_type(input)?;
	let (output, _) = multispace1(output)?;
	let (output, name) = nom_ident(output)?;
	let (output, _) = multispace0(output)?;
	let white_spaced_ident = delimited(multispace0, nom_ident, multispace0);
	let (output, params) = delimited(
		nom_char('('),
		separated_list0(nom_char(','), white_spaced_ident),
		nom_char(')'),
	)(output)?;
	let (output, _) = tuple((multispace0, nom_char(':'), multispace0))(output)?;
	let (output, inner_scope) = nom_scope(output)?;
	let (output, _) = tuple((multispace0, tag_no_case("return"), multispace0))(output)?;
	let (output, expr) = opt(nom_expression)(output)?;
	let (output, _) = pair(multispace0, nom_char(';'))(output)?;
	let function =
		Function { r#type: fn_type, name, parameters: params, scope: inner_scope, r#return: expr };
	Ok((output, function))
}

fn nom_program_item(input: &str) -> MyIResult<ProgramItem> {
	let item_fn = map(nom_function, ProgramItem::Fn);
	let item_scope = map(nom_scope, ProgramItem::Scope);
	alt((item_fn, item_scope))(input)
}

fn nom_program(input: &str) -> MyIResult<Program> {
	let (output, items) = many0(delimited(multispace0, nom_program_item, multispace0))(input)?;
	let program = Program { items };
	Ok((output, program))
}

pub fn parse_program(input: &str) -> Result<Program, Err<Error<&str>>> {
	let (rest, program) = nom_program(input)?;
	not(anychar)(rest)?;
	Ok(program)
}
