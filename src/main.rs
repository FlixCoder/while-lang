use std::fs;

use while_lang::language::Program;

fn main() -> color_eyre::Result<()> {
	let file = fs::read_to_string("program.while")?;
	let program: Program = file.parse()?;

	println!("{:#?}", program);

	Ok(())
}
