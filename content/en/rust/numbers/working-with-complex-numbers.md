---
date: 2024-01-25 02:59:36.321582-07:00
description: 'How to: Rust doesn''t have built-in complex number support, but crates
  like `num-complex` have your back. Here''s how to use it.'
lastmod: '2024-03-13T22:44:59.890198-06:00'
model: gpt-4-1106-preview
summary: Rust doesn't have built-in complex number support, but crates like `num-complex`
  have your back.
title: Working with complex numbers
weight: 14
---

## How to:
Rust doesn't have built-in complex number support, but crates like `num-complex` have your back. Here's how to use it:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Sum: {}", sum); // Sum: 3 - 1i
    println!("Product: {}", product); // Product: 14 - 5i
}
```
You'll need to add `num_complex` to your `Cargo.toml` to make this magic happen.

## Deep Dive
Complex numbers were conceived in the 16th century but really took off in the 18th century when mathematicians like Euler started playing with them. 

Without native complex number operations, languages like Rust rely on third-party libraries. `num-complex` is one such crate and is part of the `num` crate collection that aims to provide numeric types and traits for Rust. 

It's worth mentioning that some languages (like Python) have built-in support for complex numbers, while others (like C++, with the `<complex>` header) provide them as part of the standard library. In Rust, the decision to keep the standard library small means you'll often reach for community-created crates for additional functionality.

## See Also
- [Rust Book](https://doc.rust-lang.org/book/): To learn more about Rust and how to work with external crates.
- [Complex Number Wikipedia](https://en.wikipedia.org/wiki/Complex_number): For a deeper understanding of complex numbers.
