---
title:                "Rust recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is an essential part of programming, especially when working on games, simulations, or any application that requires randomization. Rust provides efficient and reliable ways to generate random numbers, making it a popular choice for many developers.

## How To

To generate random numbers in Rust, we can use the `rand` crate. First, we need to add the `rand` dependency to our `Cargo.toml` file:

```Rust
[dependencies]
rand = "0.8.3"
```

Next, we can use the `rand::Rng` trait to generate random numbers. Here's a basic example:

```Rust
use rand::Rng;

fn main() {
    let random_number = rand::thread_rng().gen_range(0..10);
    // generates a random number between 0 and 10 (exclusive)
    println!("Random number: {}", random_number);
}
```

We can also generate random numbers within a specific range, using `gen_range` or `gen_range_inclusive` methods:

```Rust
use rand::Rng;

fn main() {
    let random_number = rand::thread_rng().gen_range(1..=100);
    // generates a random number between 1 and 100 (inclusive)
    println!("Random number: {}", random_number);
}
```

To generate a random boolean value, we can use the `gen` method:

```Rust
use rand::Rng;

fn main() {
    let random_bool = rand::thread_rng().gen_bool(0.5);
    // generates a random boolean value with a 50% chance of being true
    println!("Random boolean: {}", random_bool);
}
```

The `rand::thread_rng()` function returns a `ThreadRng` object, which is a random number generator seeded by the operating system. We can also use a deterministic PRNG by explicitly seeding it using `StdRng` or `SmallRng` structs.

## Deep Dive

Behind the scenes, Rust uses the Xorshift128+ algorithm to generate random numbers. This algorithm is fast and has good statistical properties. It works by creating a sequence of random bits that are then used to generate numbers within a specific range.

It's important to note that the `rand` crate is not cryptographically secure. It should not be used for any security-sensitive applications.

To generate random numbers from a specific distribution, we can use the `Distribution` trait. This trait is implemented for many common distributions like `Uniform`, `Normal`, and `Poisson`.

```Rust
use rand::Rng;
use rand::distributions::{Uniform, Normal};

fn main() {
    let normal_dist = Normal::new(0.0, 1.0); // mean = 0, standard deviation = 1
    let num = rand::thread_rng().sample(normal_dist);
    println!("Random number from normal distribution: {}", num);
}
```

There are many other methods and techniques for generating random numbers in Rust, such as using external libraries like `rand_distr` or working with specific data types like `BigInt` and `BigUint`. Check out the `rand` crate documentation for more information.

## See Also

- [The `rand` crate documentation](https://docs.rs/rand/0.8.3/rand/)
- [Generating random numbers in Rust - tutorial by The Rust Programming Language](https://doc.rust-lang.org/stable/book/ch07-03-paths-for-referring-to-an-item-in-the-module-tree.html#paths-for-referring-to-an-item-in-the-module-tree)
- [Choosing the right RNG in Rust](https://joshuablake.medium.com/choosing-the-right-rng-in-rust-3d016c9b0340)