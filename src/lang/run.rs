use crate::{
    input::{Button, Coordinate, Input, InputError, Key, KeyboardKey, MouseButton},
    lang::{
        address::{Address, AddressRange, UAddr},
        parse::Program,
    },
    screen::{ColorRGB, Screen},
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
    InputError(InputError),
    Utf8Error(std::str::Utf8Error),
}

impl From<InputError> for RuntimeError {
    #[inline]
    fn from(e: InputError) -> Self {
        Self::InputError(e)
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
            Self::InputError(_) => write!(f, "input error"),
            Self::Utf8Error(_) => write!(f, "utf8 error"),
        }
    }
}

impl std::error::Error for RuntimeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InputError(e) => Some(e),
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

impl Source {
    fn get<'a>(&'a self, ram: &'a Memory) -> Result<&'a [u8], RuntimeError> {
        match self {
            Self::Immediate(value) => Ok(value),
            Self::Address(addr) => ram.get(*addr),
        }
    }

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
    pub fn get_mut_as<T>(&self, range: AddressRange) -> Result<&T, RuntimeError> {
        assert_eq!(range.size() as usize, std::mem::size_of::<T>());
        self.get(range)
            .map(|data| unsafe { &*(std::ptr::from_ref::<[u8]>(data).cast::<T>()) })
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
    String,
    Color,
    Key,
    Button,
    Coordinate,
    List { ty: Box<Type> },
    Array { ty: Box<Type>, count: UAddr },
    Tuple { tys: Box<[Type]> },
}

impl Type {
    #[inline]
    pub fn size(&self) -> Result<UAddr, RuntimeError> {
        use std::mem::size_of;
        match self {
            Self::Bool => Ok(size_of::<bool>() as UAddr),
            Self::Int => Ok(size_of::<i32>() as UAddr),
            Self::String => Ok(size_of::<String>() as UAddr),
            Self::Color => Ok(size_of::<ColorRGB>() as UAddr),
            Self::Key => Ok(size_of::<Key>() as UAddr),
            Self::Button => Ok(size_of::<Button>() as UAddr),
            Self::Coordinate => Ok(size_of::<Coordinate>() as UAddr),
            Self::List { .. } => Ok(size_of::<AddressRange>() as UAddr),
            Self::Array { ty, count } => ty
                .size()?
                .checked_mul(*count)
                .ok_or_else(|| RuntimeError::TypeSizeOverflow(self.clone())),
            Self::Tuple { tys } => tys.iter().map(|ty| ty.size()).sum(),
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
            Self::String => Some(Box::from(s.as_bytes())),
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
            Self::Coordinate => match s {
                "Abs" => Some(Coordinate::Abs),
                "Rel" => Some(Coordinate::Rel),
                _ => None,
            }
            .map(|x| [x as u8])
            .map(Box::from),
            Self::List { .. } => None,  // Not yet supported for literals
            Self::Array { .. } => None, // Not yet supported for literals
            Self::Tuple { .. } => None, // Not yet supported for literals
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
                    Some(Self::String)
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

#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub enum Instruction {
    Set {
        T: Type,
        /// [`T`][`Instruction::Set::T`]
        dest: AddressRange,
        /// [`T`][`Instruction::Set::T`]
        src: Source,
    },
    GetPixel {
        /// [`Type::Color`]
        dest: AddressRange,
        /// [`Type::Int`]
        x: Source,
        /// [`Type::Int`]
        y: Source,
    },
    Print {
        T: Type,
        /// [`T`][`Instruction::Print::T`]
        what: Source,
    },
    MoveMouse {
        /// [`Type::Coordinate`]
        coord: Source,
        /// [`Type::Int`]
        x: Source,
        /// [`Type::Int`]
        y: Source,
    },
    Key {
        /// [`Type::Key`]
        key: Source,
    },
    Button {
        /// [`Type::Button`]
        button: Source,
    },
}

impl Instruction {
    pub fn run(
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
            Self::GetPixel { dest, x, y } => todo!(),
            Self::Print { T, what } => match T {
                Type::Bool => println!("{}", what.get_as::<bool>(ram)?),
                Type::Int => println!("{}", what.get_as::<i32>(ram)?),
                Type::String => println!("{}", what.get_as_str(ram)?),
                Type::Color => println!("{:?}", what.get_as::<ColorRGB>(ram)?),
                Type::Key => println!("{:?}", what.get_as::<Key>(ram)?),
                Type::Button => println!("{:?}", what.get_as::<Button>(ram)?),
                Type::Coordinate => println!("{:?}", what.get_as::<Coordinate>(ram)?),
                Type::List { ty } => todo!(),
                Type::Array { ty, count } => todo!(),
                Type::Tuple { tys } => todo!(),
            },
            Self::MoveMouse { coord, x, y } => todo!(),
            Self::Key { key } => todo!(),
            Self::Button { button } => todo!(),
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
