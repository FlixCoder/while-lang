use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
	#[error("Nom parsing error: {0}")]
	Nom(String),
}

impl<'a> From<nom::Err<nom::error::Error<&'a str>>> for ParseError {
	fn from(err: nom::Err<nom::error::Error<&'a str>>) -> Self {
		Self::Nom(err.to_string())
	}
}
