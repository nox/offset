//! # Glorified offsets for arbitrary structures
//!
//! Why write `foo.bar`, when you could write `Foo::bar_offset().index_in(foo)`?

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData as marker;
use core::mem::MaybeUninit;
use core::ops::Add;

/// An offset, representing a value of type `Field` into a value of type `Base`.
///
/// The value of the offset is internally stored as an `u32`, so you are on your
/// own if you try to use that crate with a struct larger than 4 GiB.
///
/// Offsets can be added together as long as their types correspond, i.e. it is
/// possible to add an `Offset<B, C>` to an `Offset<A, B>` to get an `Offset<A, C>`.
#[repr(transparent)]
pub struct Offset<Base, Field> {
    value: u32,
    marker: marker<*const (Base, Field)>,
}
unsafe impl<Base, Field> Send for Offset<Base, Field> {}
unsafe impl<Base, Field> Sync for Offset<Base, Field> {}

impl<Base, Field> Offset<Base, Field> {
    /// Creates a new arbitrary offset.
    ///
    /// ## Safety
    ///
    /// There must be a value of type `Field` at `value` bytes away from
    /// the start of a value of type `Base`.
    #[inline(always)]
    pub const unsafe fn new_unchecked(value: u32) -> Self {
        Self { value, marker }
    }

    /// Returns an offset suitable to be used with values of type `MaybeUninit<Base>`.
    #[inline(always)]
    pub const fn uninit(self) -> Offset<MaybeUninit<Base>, MaybeUninit<Field>> {
        Offset {
            value: self.value,
            marker,
        }
    }

    /// Returns a reference to the value of type `Field` in `base` at this offset.
    #[inline(always)]
    pub fn index_in(self, base: &Base) -> &Field {
        unsafe { &*((base as *const Base as *const u8).add(self.value as usize) as *const Field) }
    }

    /// Returns a mutable reference to the value of type `Field` in `base` at this offset.
    #[inline(always)]
    pub fn index_mut_in(self, base: &mut Base) -> &mut Field {
        unsafe { &mut *((base as *mut Base as *mut u8).add(self.value as usize) as *mut Field) }
    }
}

impl<A, B, C> Add<Offset<B, C>> for Offset<A, B> {
    type Output = Offset<A, C>;

    #[inline(always)]
    fn add(self, other: Offset<B, C>) -> Self::Output {
        Offset {
            value: self.value + other.value,
            marker,
        }
    }
}

impl<Base, Field> Copy for Offset<Base, Field> {}
impl<Base, Field> Clone for Offset<Base, Field> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<Base, Field> Eq for Offset<Base, Field> {}
impl<Base, Field> PartialEq for Offset<Base, Field> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<Base, Field> PartialEq<&Self> for Offset<Base, Field> {
    #[inline(always)]
    fn eq(&self, other: &&Self) -> bool {
        self == *other
    }
}

impl<Base, Field> PartialEq<u32> for Offset<Base, Field> {
    #[inline(always)]
    fn eq(&self, other: &u32) -> bool {
        self.value == *other
    }
}

impl<Base, Field> PartialEq<&u32> for Offset<Base, Field> {
    #[inline(always)]
    fn eq(&self, other: &&u32) -> bool {
        self.value == **other
    }
}

impl<Base, Field> Ord for Offset<Base, Field> {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<Base, Field> PartialOrd for Offset<Base, Field> {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<Base, Field> PartialOrd<&Self> for Offset<Base, Field> {
    #[inline(always)]
    fn partial_cmp(&self, other: &&Self) -> Option<Ordering> {
        Some(self.cmp(&**other))
    }
}

impl<Base, Field> PartialOrd<u32> for Offset<Base, Field> {
    #[inline(always)]
    fn partial_cmp(&self, other: &u32) -> Option<Ordering> {
        self.value.partial_cmp(other)
    }
}

impl<Base, Field> PartialOrd<&u32> for Offset<Base, Field> {
    #[inline(always)]
    fn partial_cmp(&self, other: &&u32) -> Option<Ordering> {
        self.value.partial_cmp(other)
    }
}

impl<Base, Field> Hash for Offset<Base, Field> {
    #[inline(always)]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.value.hash(state);
    }
}

impl<Base, Field> fmt::Debug for Offset<Base, Field>
where
    Base: DescribeOffset,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        Base::describe_offset(self.value, fmt)
    }
}

pub trait DescribeOffset {
    fn describe_offset(offset: u32, fmt: &mut fmt::Formatter) -> fmt::Result;
}

macro_rules! trivial_fmt_impl {
    ($($t:ident),*) => {
        $(impl<Base, Field> fmt::$t for Offset<Base, Field> {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                self.value.fmt(fmt)
            }
        })*
    }
}
trivial_fmt_impl!(Binary, Display, LowerHex, Octal, UpperHex);

impl<Base, Field> From<Offset<Base, Field>> for u32 {
    #[inline(always)]
    fn from(offset: Offset<Base, Field>) -> Self {
        offset.value
    }
}
