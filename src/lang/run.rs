use crate::{
    input::{Button, Coordinate, Input, InputError, Key},
    lang::{
        address::{Address, AddressRange, UAddr},
        memory::{Memory, Source},
        parse::{Instruction, Program},
        types::Type,
    },
    screen::{ColorRGB, Screen, ScreenError},
};
use enigo::Direction;
pub use std::ops::ControlFlow;

#[derive(Debug)]
pub enum RuntimeError {
    InvalidDeref {
        addr: AddressRange,
        possible: AddressRange,
    },
    TypeSizeOverflow(Type),
    UnionSizeOverflow(u128),
    AddressOverflow(Address, UAddr),
    DivByZero,
    InputError(InputError),
    ScreenError(ScreenError),
    Utf8Error(std::str::Utf8Error),
}

impl From<InputError> for RuntimeError {
    #[inline]
    fn from(e: InputError) -> Self {
        Self::InputError(e)
    }
}

impl From<ScreenError> for RuntimeError {
    #[inline]
    fn from(e: ScreenError) -> Self {
        Self::ScreenError(e)
    }
}

impl From<std::str::Utf8Error> for RuntimeError {
    #[inline]
    fn from(e: std::str::Utf8Error) -> Self {
        Self::Utf8Error(e)
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidDeref { addr, possible } => {
                write!(
                    f,
                    "attempted to dereference the invalid address range: {addr} (ram is {possible})"
                )
            }
            Self::TypeSizeOverflow(ty) => {
                write!(
                    f,
                    "type size in bytes resulted in integer overflow. type: {ty:?}"
                )
            }
            Self::UnionSizeOverflow(count) => {
                write!(
                    f,
                    "union has more variants ({count}) than can be expressed with the largest available integer"
                )
            }
            Self::AddressOverflow(addr, size) => {
                write!(
                    f,
                    "address range resulted in integer overflow. address: {addr}, size: {size}"
                )
            }
            Self::DivByZero => write!(f, "attempted to divide by zero"),
            Self::InputError(_) => write!(f, "input error"),
            Self::ScreenError(_) => write!(f, "screen error"),
            Self::Utf8Error(_) => write!(f, "utf8 error"),
        }
    }
}

impl std::error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InputError(e) => Some(e),
            Self::ScreenError(e) => Some(e),
            Self::Utf8Error(e) => Some(e),
            _ => None,
        }
    }
}

impl Instruction {
    fn run(
        &self,
        input: &mut Input,
        screen: &mut Screen,
        ram: &mut Memory,
        counter: &mut UAddr,
    ) -> Result<(), RuntimeError> {
        match self {
            Self::Set { T, dest, src } => {
                let type_size = T.size()?;
                debug_assert_eq!(dest.size(), type_size);
                debug_assert_eq!(src.size(), type_size);
                match src {
                    Source::Immediate(val) => ram.get_mut(*dest)?.copy_from_slice(val),
                    Source::Address(src) => ram.copy(*dest, *src)?,
                }
            }
            Self::If { cond, jump } => {
                let cond = *cond.get_as::<bool>(ram)?;
                let jump = *jump.get_as::<UAddr>(ram)?;
                if !cond {
                    *counter = jump;
                    return Ok(());
                }
            }
            Self::Add { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_add(rhs);
            }
            Self::Sub { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_sub(rhs);
            }
            Self::Mul { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_mul(rhs);
            }
            Self::Div { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                if rhs != 0 {
                    *ram.get_mut_as(*dest)? = lhs.wrapping_div(rhs);
                } else {
                    return Err(RuntimeError::DivByZero);
                }
            }
            Self::Rem { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                if rhs != 0 {
                    *ram.get_mut_as(*dest)? = lhs.wrapping_rem(rhs);
                } else {
                    return Err(RuntimeError::DivByZero);
                }
            }
            Self::AddAssign { dest, rhs } => {
                let lhs = *ram.get_as::<i32>(*dest)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_add(rhs);
            }
            Self::SubAssign { dest, rhs } => {
                let lhs = *ram.get_as::<i32>(*dest)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_sub(rhs);
            }
            Self::MulAssign { dest, rhs } => {
                let lhs = *ram.get_as::<i32>(*dest)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                *ram.get_mut_as(*dest)? = lhs.wrapping_mul(rhs);
            }
            Self::DivAssign { dest, rhs } => {
                let lhs = *ram.get_as::<i32>(*dest)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                if rhs != 0 {
                    *ram.get_mut_as(*dest)? = lhs.wrapping_div(rhs);
                } else {
                    return Err(RuntimeError::DivByZero);
                }
            }
            Self::RemAssign { dest, rhs } => {
                let lhs = *ram.get_as::<i32>(*dest)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                if rhs != 0 {
                    *ram.get_mut_as(*dest)? = lhs.wrapping_rem(rhs);
                } else {
                    return Err(RuntimeError::DivByZero);
                }
            }
            Self::GetPixel { dest, x, y } => {
                let x = *x.get_as::<i32>(ram)?;
                let y = *y.get_as::<i32>(ram)?;
                let color = unsafe { screen.get_pixel(x, y) }
                    .map_err(RuntimeError::ScreenError)?
                    .to_rgb();
                *ram.get_mut_as(*dest)? = color;
            }
            Self::Print { T, what } => {
                fn print(ty: &Type, what: &Source, ram: &Memory) -> Result<(), RuntimeError> {
                    match ty {
                        Type::Bool => println!("{}", what.get_as::<bool>(ram)?),
                        Type::Int => println!("{}", what.get_as::<i32>(ram)?),
                        Type::Label => println!("0x{:08x}", what.get_as::<UAddr>(ram)?),
                        Type::Str => println!("{}", what.get_as_str(ram)?),
                        Type::Color => println!("{:?}", what.get_as::<ColorRGB>(ram)?),
                        Type::Key => println!("{:?}", what.get_as::<Key>(ram)?),
                        Type::Button => println!("{:?}", what.get_as::<Button>(ram)?),
                        Type::Action => println!("{:?}", what.get_as::<Direction>(ram)?),
                        Type::Coordinate => println!("{:?}", what.get_as::<Coordinate>(ram)?),
                        Type::Union(tys) => {
                            let (ty, bytes) = Type::union_variant(tys, what.get(ram)?)?;
                            print(ty, &Source::Immediate(Box::from(bytes)), ram)?;
                        }
                    }
                    Ok(())
                }
                print(T, what, ram)?;
            }
            Self::MoveMouse { coord, x, y } => {
                let coord = *coord.get_as::<Coordinate>(ram)?;
                let x = *x.get_as::<i32>(ram)?;
                let y = *y.get_as::<i32>(ram)?;
                input.move_mouse(x, y, coord)?;
            }
            Self::Key { action, key } => {
                let action = *action.get_as::<Direction>(ram)?;
                let key = *key.get_as::<Key>(ram)?;
                input.key(key, action)?;
            }
            Self::Button { action, button } => {
                let action = *action.get_as::<Direction>(ram)?;
                let button = *button.get_as::<Button>(ram)?;
                input.button(button, action)?;
            }
        }
        *counter += 1;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Runner<'a> {
    memory: Memory,
    prgm: &'a Program,
    counter: UAddr,
}

impl Runner<'_> {
    pub fn from_program(prgm: &Program, ram_capacity: UAddr) -> Runner<'_> {
        Runner {
            memory: Memory::with_capacity(ram_capacity),
            prgm,
            counter: 0,
        }
    }

    pub fn step(
        &mut self,
        input: &mut Input,
        screen: &mut Screen,
    ) -> ControlFlow<Result<(), RuntimeError>, ()> {
        match self.prgm.line(self.counter) {
            Some(line) => {
                if let Err(e) = line.run(input, screen, &mut self.memory, &mut self.counter) {
                    return ControlFlow::Break(Err(e));
                }
                ControlFlow::Continue(())
            }
            None => ControlFlow::Break(Ok(())),
        }
    }
}
