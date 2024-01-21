---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:47.355528-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are unpredictable values. Programmers use them for stuff like games, simulations, and security (ever heard of encryption?).

## How to:
Rust's `rand` crate is the go-to for random numbers. First, add `rand` to your `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

Now the fun part: generating some numbers!

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();

    // Random f64 from 0 to 1
    let x: f64 = rng.gen();
    println!("Random f64: {}", x);

    // Random i32
    let y: i32 = rng.gen();
    println!("Random i32: {}", y);

    // Random bool
    let z: bool = rng.gen();
    println!("Random bool: {}", z);
}
```

Output - expect something unexpected:

```
Random f64: 0.840938355033784
Random i32: 1690851234
Random bool: false
```

## Deep Dive

Before `rand`, folks used primitive methods like the linear congruential generator. “What's that?”, you might ask. Old-school math for pseudo-random sequences. They were good, but had patterns. Easy to guess, not so great for crypto.

Now, `rand` uses more complex algorithms. `thread_rng`, for example, hooks into your operating system's random features to get good-quality randomness, suitable for cryptography.

There's more than one way to skin a cat, though: Rust also offers `rand_pcg`, `rand_xoshiro`, etc., for when you need different performance or randomness characteristics.

Implementation-wise, `rand` uses traits like `Rng` and `SeedableRng`. Traits define behavior. `Rng` says, "This type can generate random numbers," and `SeedableRng` is for when you want to create a reproducible sequence of "random" numbers (helpful for testing).

## See Also

Check out these links for more brain food:

- The `rand` crate's documentation: https://docs.rs/rand
- Rust's trait system: https://doc.rust-lang.org/book/ch10-02-traits.html
- Cryptographically secure randomness: https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator