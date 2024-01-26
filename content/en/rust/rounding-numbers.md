---
title:                "Rounding numbers"
date:                  2024-01-25T03:00:16.895639-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to the nearest whole number or a fraction with a certain precision. Programmers round numbers to simplify values for human readability, to meet specification requirements, or to reduce computational overhead in floating-point operations.

## How to:
Rust makes rounding a breeze. Check these methods out for `f32` or `f64` types:

```rust
fn main() {
    let num = 2.34567;

    // Round to nearest whole number
    let round = num.round();
    println!("Round: {}", round); // Round: 2

    // Floor - largest integer less than or equal to number
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - smallest integer greater than or equal to number
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - integer part without fractional digits
    let trunc = num.trunc();
    println!("Truncate: {}", trunc); // Truncate: 2

    // To the nearest multiple of a power of ten
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Rounded to 2 decimal places: {}", multiple_of_ten); // Rounded to 2 decimal places: 2.35
}
```

## Deep Dive
Historically, rounding's been crucial for fitting infinite decimals or irrational numbers in limited digital spaces—a must for ancient computers with scant memory. Think abacus but less crafty, more maths.

Alternatives to the native Rust methods include:
1. `format!` macro for string formatting that rounds by default.
2. External crates for specialized math tasks, like `round` crate with more granular control.

Under the hood, Rust's rounding ops comply with IEEE standards—tech jargon for "it rounds like your math teacher wants." Plus, because of binary representations, some numbers can't be rounded traditionally, like 0.1, due to their infinite representation in binary.

## See Also
- Rust doc on primitive type methods: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE Standard for Floating-Point Arithmetic (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round" crate for more complex rounding: https://crates.io/crates/round
