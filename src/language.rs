use std::str::FromStr;

use crate::parse::parse_program;

#[derive(Debug)]
pub struct Program {
	pub items: Vec<ProgramItem>,
}

impl FromStr for Program {
	type Err = color_eyre::Report;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		parse_program(s)
	}
}

#[derive(Debug)]
pub enum ProgramItem {
	Fn(Function),
	Scope(Scope),
}

#[derive(Debug)]
pub struct Function {
	pub r#type: FnType,
	pub name: Ident,
	pub parameters: Vec<Ident>,
	pub scope: Scope,
	pub r#return: Option<Expression>,
}

#[derive(Debug)]
pub struct Scope {
	pub items: Vec<ScopeItem>,
}

#[derive(Debug)]
pub enum ScopeItem {
	Assignment(Assignment),
	Increment(Increment),
	Decrement(Decrement),
	FnCall(FnCall),
	While(While),
}

#[derive(Debug)]
pub struct While {
	pub variable: Ident,
	pub not: Digit,
	pub scope: Scope,
}

#[derive(Debug)]
pub struct Assignment {
	pub variable: Ident,
	pub expression: Expression,
}

#[derive(Debug)]
pub struct Increment {
	pub variable: Ident,
	pub expression: Expression,
}

#[derive(Debug)]
pub struct Decrement {
	pub variable: Ident,
	pub expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
	Variable(Ident),
	Digit(Digit),
	FnCall(FnCall),
}

#[derive(Debug)]
pub struct FnCall {
	pub name: Ident,
	pub parameters: Vec<Expression>,
}

#[derive(Debug)]
pub enum FnType {
	Num,
}

#[derive(Debug)]
pub struct Ident(pub String);

#[derive(Debug)]
pub struct Digit(pub usize);
