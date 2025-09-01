use enigo::Direction;

use crate::{
    input::{Button, Coordinate, Input, InputError, Key, KeyboardKey, MouseButton},
    lang::{
        address::{Address, AddressRange, UAddr},
        parse::{Instruction, Program},
    },
    screen::{ColorRGB, Screen, ScreenError},
};
pub use std::ops::ControlFlow;

#[derive(Debug)]
pub enum RuntimeError {
    InvalidDeref {
        addr: AddressRange,
        possible: AddressRange,
    },
    TypeSizeOverflow(Type),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Source {
    Immediate(Box<[u8]>),
    Address(AddressRange),
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Immediate(value) => write!(f, "${value:?}"),
            Self::Address(addr) => write!(f, "{addr}"),
        }
    }
}

impl Source {
    fn get_as<'a, T>(&'a self, ram: &'a Memory) -> Result<&'a T, RuntimeError> {
        match self {
            Self::Immediate(value) => {
                assert_eq!(self.size() as usize, std::mem::size_of::<T>());
                Ok(unsafe { &*std::ptr::from_ref::<[u8]>(value).cast::<T>() })
            }
            Self::Address(addr) => ram.get_as(*addr),
        }
    }

    fn get_as_str<'a>(&'a self, ram: &'a Memory) -> Result<&'a str, RuntimeError> {
        match self {
            Self::Immediate(value) => Ok(str::from_utf8(value)?),
            Self::Address(addr) => ram.get_as_str(*addr),
        }
    }

    pub fn size(&self) -> UAddr {
        match self {
            Self::Immediate(value) => value
                .len()
                .try_into()
                .expect("should be guaranteed by invariant"),
            Self::Address(addr) => addr.size(),
        }
    }
}

#[derive(Debug, Clone)]
struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: vec![0; capacity],
        }
    }

    #[inline]
    pub fn get(&self, range: AddressRange) -> Result<&[u8], RuntimeError> {
        let len = self.data.len();
        self.data
            .get(range.memory())
            .ok_or(RuntimeError::InvalidDeref {
                addr: range,
                possible: AddressRange::range_to(Address(
                    len.try_into().expect("should be invariant"),
                )),
            })
    }

    #[inline]
    pub fn get_mut(&mut self, range: AddressRange) -> Result<&mut [u8], RuntimeError> {
        let len = self.data.len();
        self.data
            .get_mut(range.memory())
            .ok_or(RuntimeError::InvalidDeref {
                addr: range,
                possible: AddressRange::range_to(Address(
                    len.try_into().expect("should be invariant"),
                )),
            })
    }

    #[inline]
    pub fn get_as<T>(&self, range: AddressRange) -> Result<&T, RuntimeError> {
        assert_eq!(range.size() as usize, std::mem::size_of::<T>());
        self.get(range)
            .map(|data| unsafe { &*(std::ptr::from_ref::<[u8]>(data).cast::<T>()) })
    }

    #[inline]
    pub fn get_as_str(&self, range: AddressRange) -> Result<&str, RuntimeError> {
        self.get(range)
            .and_then(|data| str::from_utf8(data).map_err(RuntimeError::from))
    }

    #[inline]
    pub fn get_mut_as<T>(&mut self, range: AddressRange) -> Result<&mut T, RuntimeError> {
        assert_eq!(range.size() as usize, std::mem::size_of::<T>());
        self.get_mut(range)
            .map(|data| unsafe { &mut *(std::ptr::from_mut::<[u8]>(data).cast::<T>()) })
    }

    #[inline]
    pub fn copy(&mut self, dest: AddressRange, src: AddressRange) -> Result<(), RuntimeError> {
        self.data.copy_within(src.memory(), dest.memory().start);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int,
    Str,
    Color,
    Key,
    Button,
    Action,
    Coordinate,
}

impl Type {
    #[inline]
    pub fn size(&self) -> Result<UAddr, RuntimeError> {
        use std::mem::size_of;
        match self {
            Self::Bool => Ok(size_of::<bool>() as UAddr),
            Self::Int => Ok(size_of::<i32>() as UAddr),
            Self::Str => Ok(size_of::<String>() as UAddr),
            Self::Color => Ok(size_of::<ColorRGB>() as UAddr),
            Self::Key => Ok(size_of::<Key>() as UAddr),
            Self::Button => Ok(size_of::<Button>() as UAddr),
            Self::Action => Ok(size_of::<Direction>() as UAddr),
            Self::Coordinate => Ok(size_of::<Coordinate>() as UAddr),
        }
    }

    pub fn try_parse_imm(&self, s: &str) -> Option<Box<[u8]>> {
        match self {
            Self::Bool => match s {
                "True" => Some(true),
                "False" => Some(false),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::Int => s.parse::<i32>().ok().map(i32::to_ne_bytes).map(Box::from),
            Self::Str => Some(Box::from(s.as_bytes())),
            Self::Color => s
                .parse::<ColorRGB>()
                .ok()
                .map(|x| [x.r, x.g, x.b])
                .map(Box::from),
            Self::Key => s
                .parse::<KeyboardKey>()
                .ok()
                .map(KeyboardKey::to_ne_bytes)
                .map(Box::from),
            Self::Button => s
                .parse::<MouseButton>()
                .ok()
                .map(MouseButton::to_ne_bytes)
                .map(Box::from),
            Self::Action => match s {
                "Press" => Some(Direction::Press),
                "Release" => Some(Direction::Release),
                "Click" => Some(Direction::Click),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::Coordinate => match s {
                "Abs" => Some(Coordinate::Abs),
                "Rel" => Some(Coordinate::Rel),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
        }
    }

    pub fn try_deduce_imm(s: &str) -> Option<Self> {
        match s {
            "True" | "False" => Some(Self::Bool),
            "Abs" | "Rel" => Some(Self::Coordinate),
            _ => {
                if s.parse::<i32>().is_ok() {
                    Some(Self::Int)
                } else if s.starts_with('#') {
                    Some(Self::Color)
                } else if s.starts_with('"') && s.ends_with('"') {
                    Some(Self::Str)
                } else if s.parse::<KeyboardKey>().is_ok() {
                    Some(Self::Key)
                } else if s.parse::<MouseButton>().is_ok() {
                    Some(Self::Button)
                } else {
                    None // todo
                }
            }
        }
    }
}

impl Instruction {
    fn run(
        &self,
        input: &mut Input,
        screen: &mut Screen,
        ram: &mut Memory,
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
            Self::Print { T, what } => match T {
                Type::Bool => println!("{}", what.get_as::<bool>(ram)?),
                Type::Int => println!("{}", what.get_as::<i32>(ram)?),
                Type::Str => println!("{}", what.get_as_str(ram)?),
                Type::Color => println!("{:?}", what.get_as::<ColorRGB>(ram)?),
                Type::Key => println!("{:?}", what.get_as::<Key>(ram)?),
                Type::Button => println!("{:?}", what.get_as::<Button>(ram)?),
                Type::Action => println!("{:?}", what.get_as::<Direction>(ram)?),
                Type::Coordinate => println!("{:?}", what.get_as::<Coordinate>(ram)?),
            },
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
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Runner<'a> {
    memory: Memory,
    prgm: &'a Program,
    counter: usize,
}

impl Runner<'_> {
    pub fn from_program(prgm: &Program, ram_capacity: usize) -> Runner<'_> {
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
                if let Err(e) = line.run(input, screen, &mut self.memory) {
                    return ControlFlow::Break(Err(e));
                }
                self.counter += 1;
                ControlFlow::Continue(())
            }
            None => ControlFlow::Break(Ok(())),
        }
    }
}
