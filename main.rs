use derive_debug::CustomDebug;
use std::{fmt::Debug, marker::PhantomData};

pub trait Trait {
    type Value;
}

#[derive(CustomDebug)]
pub struct Field<T: Trait> {
    values: Vec<T::Value>,
}

#[derive(CustomDebug)]
pub struct One<T, K>
where
    K: Eq,
{
    value: T,
    k: PhantomData<K>,
    two: Option<Box<Two<T>>>,
}

#[derive(CustomDebug)]
struct Two<T> {
    one: Box<One<T, usize>>,
}

fn assert_debug<F: Debug>() {}

fn main() {
    // Does not implement Debug, but its associated type does.
    struct Id;

    impl Trait for Id {
        type Value = u8;
    }

    assert_debug::<Field<Id>>();
    assert_debug::<One<u8, u16>>();
    assert_debug::<Two<u8>>();
}
