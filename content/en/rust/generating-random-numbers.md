---
title:                "Generating random numbers"
html_title:           "Rust recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is the process of producing a sequence of numbers that do not follow any predictable pattern. Programmers use random numbers in various applications, such as simulations, games, and cryptography, to add an element of randomness and unpredictability.

## How to:
Generating random numbers in Rust is made easy with the standard library's `rand` crate. First, we need to add the following line to the `Cargo.toml` file under `[dependencies]`:
```
rand = "0.7.3"
```
Next, we can use the `rand::thread_rng` function to get a thread-local RNG (random number generator) and call its `gen` method to generate a random number. Here's an example:
```
use rand::prelude::*;

fn main() {
    let mut rng = thread_rng();
    let num = rng.gen::<u32>(); // generates a random number of type u32
    println!("Random number: {}", num);
}
```
Sample output:
```
Random number: 937361632
```

We can also generate random numbers within a range by using the `gen_range` method:
```
use rand::prelude::*;

fn main() {
    let mut rng = thread_rng();
    let num = rng.gen_range(1..=10); // generates a random number between 1 and 10 (inclusive)
    println!("Random number: {}", num);
}
```
Sample output:
```
Random number: 8
```

## Deep Dive:
The `rand` crate in Rust is based on the `rand_core` crate, which provides the core functionality for generating random numbers. The `rand` crate also supports features like random distributions, which allow you to generate numbers from a specific distribution, such as a normal distribution or a Bernoulli distribution.

Before the `rand` crate, Rust had a `rand` module in its standard library, but it was deprecated in favor of the `rand` crate due to its better design and maintenance. Other alternatives for generating random numbers in Rust include the `rand_xorshift` and `fastrand` crates, which use different algorithms for random number generation.

When generating random numbers, it's important to use a cryptographically secure RNG if security is a concern. The `rand` crate provides a `rand::rngs::ThreadRng` type, which uses the `OsRng` as its source of randomness, making it a secure option for generating random numbers.

## See Also:
- Official documentation for the `rand` crate: https://docs.rs/rand/
- Rust Standard Library documentation for the thread-safe RNG: https://doc.rust-lang.org/std/rand/struct.ThreadRng.html
- Rust subreddit discussion on generating random numbers: https://www.reddit.com/r/rust/comments/9olq5q/how_does_random_numbers_in_rust_work/