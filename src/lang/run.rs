use crate::{
    input::{Button, Coordinate, Input, InputError, Key},
    lang::{
        address::{Address, AddressRange, UAddr},
        instructions::Instruction,
        memory::{Memory, Source},
        parse::Program,
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
    DerefNonPointer(Type),
    TypeSizeOverflow(Type),
    UnionSizeOverflow(u128),
    AddressOverflow(Address, UAddr),
    DivByZero,
    InputError(InputError),
    ScreenError(ScreenError),
    Utf8Error(std::str::Utf8Error),
    Unknown,
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
            Self::DerefNonPointer(ty) => {
                write!(f, "attempted to dereference a non-pointer: {ty:?}")
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
            Self::Unknown => write!(f, "unknown error"),
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
            Self::Set { T: _, dest, src } => {
                let src = src.get(ram)?.to_vec();
                let dest = dest.get_mut(ram)?;

                dest.copy_from_slice(&src);
            }
            Self::Deref { T: _, dest, src } => {
                let src = src.get_as::<AddressRange>(ram)?.get(ram)?.to_vec();
                let dest = dest.get_mut(ram)?;

                dest.copy_from_slice(&src);
            }
            Self::AddInts { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<i32>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                let dest = dest.get_mut_as::<i32>(ram)?;

                *dest = lhs.wrapping_add(rhs);
            }
            Self::AddColors { dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<ColorRGB>(ram)?;
                let rhs = *rhs.get_as::<ColorRGB>(ram)?;
                let dest = dest.get_mut_as::<ColorRGB>(ram)?;

                dest.r = lhs.r.wrapping_add(rhs.r);
                dest.g = lhs.g.wrapping_add(rhs.g);
                dest.b = lhs.b.wrapping_add(rhs.b);
            }
            Self::AddPtrInt { T, dest, lhs, rhs } => {
                let lhs = *lhs.get_as::<AddressRange>(ram)?;
                let rhs = *rhs.get_as::<i32>(ram)?;
                let dest = dest.get_mut_as::<AddressRange>(ram)?;
                let step = T.size()?;

                *dest = AddressRange::new(
                    Address(lhs.start().0.wrapping_add_signed(rhs * step as i32)),
                    Address(lhs.end().0.wrapping_add_signed(rhs * step as i32)),
                )
                .ok_or(RuntimeError::Unknown)?;
            }
            Self::AddAssignInts { dest, rhs } => {
                let rhs = *rhs.get_as::<i32>(ram)?;
                let dest = dest.get_mut_as::<i32>(ram)?;

                *dest = dest.wrapping_add(rhs);
            }
            Self::AddAssignColors { dest, rhs } => {
                let rhs = *rhs.get_as::<ColorRGB>(ram)?;
                let dest = dest.get_mut_as::<ColorRGB>(ram)?;

                dest.r = dest.r.wrapping_add(rhs.r);
                dest.g = dest.g.wrapping_add(rhs.g);
                dest.b = dest.b.wrapping_add(rhs.b);
            }
            Self::AddAssignPtrInt { T, dest, rhs } => {
                let rhs = *rhs.get_as::<i32>(ram)?;
                let dest = dest.get_mut_as::<AddressRange>(ram)?;
                let step = T.size()?;

                *dest = AddressRange::new(
                    Address(dest.start().0.wrapping_add_signed(rhs * step as i32)),
                    Address(dest.end().0.wrapping_add_signed(rhs * step as i32)),
                )
                .ok_or(RuntimeError::Unknown)?;
            }
            Self::GetPixel { dest, x, y } => {
                let x = *x.get_as::<i32>(ram)?;
                let y = *y.get_as::<i32>(ram)?;
                let dest = dest.get_mut_as::<ColorRGB>(ram)?;

                *dest = unsafe { screen.get_pixel(x, y) }
                    .map_err(RuntimeError::ScreenError)?
                    .to_rgb();
            }
            Self::Print { what } => {
                /// recursive printing
                fn print(ty: &Type, what: &Source, ram: &Memory) -> Result<(), RuntimeError> {
                    match ty {
                        Type::Bool => print!("{}", what.get_as::<bool>(ram)?),
                        Type::Int => print!("{}", what.get_as::<i32>(ram)?),
                        Type::Str => print!("{}", what.get_as_str(ram)?),
                        Type::Color => print!("{:?}", what.get_as::<ColorRGB>(ram)?),
                        Type::Key => print!("{:?}", what.get_as::<Key>(ram)?),
                        Type::Button => print!("{:?}", what.get_as::<Button>(ram)?),
                        Type::Action => print!("{:?}", what.get_as::<Direction>(ram)?),
                        Type::Coordinate => print!("{:?}", what.get_as::<Coordinate>(ram)?),
                        Type::Union(tys) => {
                            let (ty, bytes) = Type::union_variant(tys, what.get(ram)?)?;
                            print(ty, &Source::Immediate(Box::from(bytes)), ram)?;
                        }
                        Type::Pointer(_) => print!("{}", what.get_as::<AddressRange>(ram)?),
                    }
                    Ok(())
                }

                for (ty, msg) in what {
                    print(ty, msg, ram)?;
                }
                println!();
            }
            Self::Wait { ms } => {
                let ms = (*ms.get_as::<i32>(ram)?).max(0);

                std::thread::sleep(std::time::Duration::from_millis(ms.try_into().unwrap()));
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
