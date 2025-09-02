use crate::lang::{address::UAddr, instructions::Instruction};

pub mod compile;
pub mod lex;
pub mod parse;

#[derive(Debug, Clone)]
pub struct Program(Box<[Instruction]>);

impl Program {
    #[inline]
    pub fn line(&self, n: UAddr) -> Option<&Instruction> {
        self.0.get(n as usize)
    }
}
