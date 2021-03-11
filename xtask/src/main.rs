//! See https://github.com/matklad/cargo-xtask/.
//!
//! This binary defines `cargo xtask codegen` for code generation.
//!
//! This binary is integrated into the `cargo` command line by using an alias in
//! `.cargo/config`.

use pico_args::Arguments;
use xshell::pushd;
use xtask::{codegen, project_root, Result};

fn main() -> Result<()> {
    let _d = pushd(project_root())?;

    let mut args = Arguments::from_env();
    let subcommand = args.subcommand()?.unwrap_or_default();

    match subcommand.as_str() {
        "codegen" => {
            args.finish()?;
            codegen::run()
        }
        _ => {
            eprintln!(
                "\
cargo xtask
Run custom build command.
USAGE:
    cargo xtask <SUBCOMMAND>
SUBCOMMANDS:
    format
    codegen"
            );
            Ok(())
        }
    }
}
