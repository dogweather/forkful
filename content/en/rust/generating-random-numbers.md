---
title:    "Rust recipe: Generating random numbers"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
When writing software, there are often times where you need to generate random numbers. Whether it's for creating unique keys, shuffling a deck of cards, or simulating a game, random numbers are an essential tool for programmers.

## How To
Generating random numbers in Rust is straightforward and can be done using the rand crate. First, you need to add the crate to your dependencies in the `Cargo.toml` file:

```Rust
[dependencies]
rand = "0.8.3"
```

Next, you can use the `rand::random` function to generate random numbers of different data types:

```Rust
let random_int: u32 = rand::random(); // Generates a random unsigned 32-bit integer
let random_float: f64 = rand::random(); // Generates a random 64-bit floating-point number
let random_bool: bool = rand::random(); // Generates a random boolean value (true or false)
```

You can also use the `rand::thread_rng` function to create a random number generator, which can be used to generate multiple numbers or create a sequence of random numbers:

```Rust
use rand::Rng;

let mut rng = rand::thread_rng();
for _ in 0..10 {
  let random_num: u8 = rng.gen(); // Generates a random unsigned 8-bit integer
  println!("{}", random_num);
}
```

The output will be 10 random numbers between 0 and 255.

## Deep Dive
Behind the scenes, Rust uses a pseudo-random number generator (PRNG) called the XorShift generator. This is a fast and efficient PRNG that generates numbers by using bitwise operations on a seed value. The `rand` crate also provides other PRNGs that you can use, such as the ChaCha20 generator.

It's worth noting that PRNGs are not truly random, as they use a mathematical formula to generate numbers. This means that the numbers they produce are deterministic and can be repeated if the same seed value is used. To generate truly random numbers, you would need to use a hardware random number generator.

## See Also
- [The `rand` crate documentation](https://docs.rs/rand/0.8.3/rand/)
- [The Rust book chapter on randomness](https://doc.rust-lang.org/book/ch07-01-mod-and-the-use-keyword.html#seeding-the-rng)
- [The PRNG article on Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)