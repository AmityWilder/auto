use crate::lang::{
    address::{Address, AddressRange, UAddr},
    run::RuntimeError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedSource<T> {
    Immediate(T),
    Address(Address),
}

impl<T: Default> Default for TypedSource<T> {
    fn default() -> Self {
        Self::Immediate(T::default())
    }
}

impl<T> TypedSource<T> {
    pub fn get<'a>(&'a self, ram: &'a Memory) -> Result<&'a T, RuntimeError> {
        match self {
            Self::Immediate(value) => Ok(value),
            Self::Address(addr) => ram.get_as::<T>(
                addr.range(
                    size_of::<T>()
                        .try_into()
                        .map_err(|_| RuntimeError::TypeSizeOverflow(todo!()))?,
                )?,
            ),
        }
    }

    pub fn get_mut<'a>(&'a mut self, ram: &'a mut Memory) -> Result<&'a mut T, RuntimeError> {
        match self {
            Self::Immediate(value) => Ok(value),
            Self::Address(addr) => ram.get_mut_as::<T>(
                addr.range(
                    size_of::<T>()
                        .try_into()
                        .map_err(|_| RuntimeError::TypeSizeOverflow(todo!()))?,
                )?,
            ),
        }
    }
}

pub trait TypedSourceDisplay {}

impl std::fmt::Display for TypedSource<bool> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Immediate(value) => match value {
                true => "True",
                false => "False",
            },
            Self::Address(addr) => addr.fmt(f),
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
            Self::Immediate(value) => {
                write!(f, "[")?;
                let mut has_prefix = false;
                for byte in value {
                    if has_prefix {
                        write!(f, " ")?;
                    } else {
                        has_prefix = true;
                    }
                    write!(f, "{:02x}", byte)?;
                }
                write!(f, "]")
            }
            Self::Address(addr) => addr.fmt(f),
        }
    }
}

impl Source {
    pub fn get<'a>(&'a self, ram: &'a Memory) -> Result<&'a [u8], RuntimeError> {
        match self {
            Self::Immediate(value) => Ok(value),
            Self::Address(addr) => ram.get(*addr),
        }
    }

    pub fn get_as<'a, T>(&'a self, ram: &'a Memory) -> Result<&'a T, RuntimeError> {
        match self {
            Self::Immediate(value) => {
                assert_eq!(self.size() as usize, std::mem::size_of::<T>());
                Ok(unsafe { &*std::ptr::from_ref::<[u8]>(value).cast::<T>() })
            }
            Self::Address(addr) => ram.get_as(*addr),
        }
    }

    pub fn get_as_str<'a>(&'a self, ram: &'a Memory) -> Result<&'a str, RuntimeError> {
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
pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn with_capacity(capacity: UAddr) -> Self {
        Self {
            data: vec![0; capacity as usize],
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
}
