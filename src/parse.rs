use color_eyre::eyre::eyre;
use nom::{
	branch::alt,
	bytes::complete::{tag, tag_no_case},
	character::complete::{
		alpha1, alphanumeric0, anychar, char as nom_char, digit1, multispace0, multispace1,
	},
	combinator::{map, map_res, not, opt, recognize},
	error::{convert_error, VerboseError},
	multi::{many0, many1, separated_list0},
	sequence::{delimited, pair, terminated, tuple},
	Finish,
};

use crate::language::*;

type MyIResult<'a, O> = nom::IResult<&'a str, O, VerboseError<&'a str>>;

fn nom_digit(input: &str) -> MyIResult<Digit> {
	map(map_res(digit1, |digit: &str| digit.parse()), Digit)(input)
}

fn nom_ident(input: &str) -> MyIResult<Ident> {
	map(recognize(pair(alpha1, alphanumeric0)), |ident: &str| Ident(ident.to_owned()))(input)
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
	alt((expr_fn_call, expr_digit, expr_ident))(input)
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
	let item_fn = map(terminated(nom_fn_call, nom_char(';')), ScopeItem::FnCall);
	let item_while = map(nom_while, ScopeItem::While);
	alt((item_fn, item_while, item_assign, item_increment, item_decrement))(input)
}

fn nom_scope(input: &str) -> MyIResult<Scope> {
	let (output, items) = many1(delimited(multispace0, nom_scope_item, multispace0))(input)?;
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
	let (rest, items) = many0(delimited(multispace0, nom_program_item, multispace0))(input)?;
	let (output, _) = not(anychar)(rest)?;
	let program = Program { items };
	Ok((output, program))
}

pub fn parse_program(input: &str) -> color_eyre::Result<Program> {
	let (_, program) =
		nom_program(input).finish().map_err(|err| eyre!(convert_error(input, err)))?;
	Ok(program)
}
