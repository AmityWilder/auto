use crate::lang::run::RuntimeError;

pub type UAddr = u32;
pub type IAddr = i32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address(pub UAddr);

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:01$x}", self.0, (UAddr::BITS / 4) as usize)
    }
}

#[cfg(target_pointer_width = "32")] // assumes UAddr is 32 bits
impl From<usize> for Address {
    #[inline]
    fn from(value: usize) -> Self {
        Self(value as UAddr)
    }
}

#[cfg(not(target_pointer_width = "16"))] // 16bit not supported
#[cfg(not(target_pointer_width = "32"))] // assumes UAddr is 32 bits
impl TryFrom<usize> for Address {
    type Error = std::num::TryFromIntError;

    #[inline]
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        UAddr::try_from(value).map(Address)
    }
}

#[cfg(not(target_pointer_width = "16"))] // 16bit not supported
impl From<Address> for usize {
    #[inline]
    fn from(value: Address) -> Self {
        value.0 as usize
    }
}

impl Address {
    #[inline]
    pub fn range(self, size: UAddr) -> Result<AddressRange, RuntimeError> {
        let start = self;
        let end = start
            .0
            .checked_add(size)
            .map(Address)
            .ok_or_else(|| RuntimeError::AddressOverflow(start, size))?;
        // SAFETY: Would have returned an error if overflow occurred
        debug_assert!(end >= start);
        Ok(unsafe { AddressRange::new_unchecked(start, end) })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AddressRange {
    start: Address,
    end: Address,
}

impl std::ops::RangeBounds<Address> for AddressRange {
    #[inline]
    fn start_bound(&self) -> std::ops::Bound<&Address> {
        std::ops::Bound::Included(&self.start)
    }

    #[inline]
    fn end_bound(&self) -> std::ops::Bound<&Address> {
        std::ops::Bound::Excluded(&self.end)
    }
}

impl std::fmt::Display for AddressRange {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl AddressRange {
    /// # Safety
    ///
    /// Requires that `end >= start`
    #[inline]
    pub const unsafe fn new_unchecked(start: Address, end: Address) -> Self {
        Self { start, end }
    }

    #[inline]
    pub const fn range_to(end: Address) -> Self {
        Self {
            start: Address(0),
            end,
        }
    }

    pub const fn new(start: Address, end: Address) -> Option<Self> {
        if end.0 >= start.0 {
            Some(Self { start, end })
        } else {
            None
        }
    }

    #[inline]
    pub const fn start(self) -> Address {
        self.start
    }

    #[inline]
    pub const fn end(self) -> Address {
        self.end
    }

    pub const fn split_at(self, size: UAddr) -> Option<(Self, Self)> {
        if size <= self.size() {
            let Self { start, end } = self;
            // SAFETY: self.end is already AT LEAST self.start + size
            let mid = Address(unsafe { start.0.unchecked_add(size) });
            Some((Self { start, end: mid }, Self { start: mid, end }))
        } else {
            None
        }
    }

    #[inline]
    pub fn memory(self) -> std::ops::Range<usize> {
        usize::from(self.start)..usize::from(self.end)
    }

    #[inline]
    pub const fn size(self) -> UAddr {
        // SAFETY: Guaranteed by type invariant
        unsafe { self.end.0.unchecked_sub(self.start.0) }
    }
}
