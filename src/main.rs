use crate::input::*;
use crate::lang::{parse::*, run::*};
use crate::screen::*;

mod input;
mod lang;
mod screen;

fn main() {
    let mut input = Input::new().unwrap();
    let mut screen = unsafe { Screen::new(None) }.unwrap();

    match include_str!("../example.txt").parse::<Program>() {
        Ok(prgm) => {
            let mut runner = Runner::from_program(&prgm, 1024);
            let exit_status = loop {
                if let ControlFlow::Break(result) = runner.step(&mut input, &mut screen) {
                    break result;
                }
            };
            match exit_status {
                Ok(()) => println!("closed with success"),
                Err(e) => println!("runtime error: {e}"),
            }
        }
        Err(e) => {
            println!("parse error: {e}");
        }
    }
}
