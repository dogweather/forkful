---
title:                "ランダムな数字の生成"
html_title:           "Rust: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers are important in programming for a variety of reasons. They can be used to simulate unpredictable events, create unique identifiers, or generate encryption keys. In Rust, the standard library provides a simple and efficient way to generate random numbers, making it a useful language for tasks that require this functionality.

## How To

To generate random numbers in Rust, we first need to import the `rand` crate. This crate provides various functions and types for generating random values.
```
Rust
use rand::Rng;
```
Next, we can use the `thread_rng` function to create a thread-local random number generator. This ensures that each thread has its own source of randomness, avoiding issues with concurrent access.
```
Rust
let mut rng = rand::thread_rng();
```
Now, we can use the `gen_range` function to generate random numbers within a given range.
```
Rust
let num: i32 = rng.gen_range(1..=10); // generates a random number between 1 and 10 (inclusive)
```
We can also use the `gen` function to generate random values of different types, such as booleans or characters.
```
Rust
let boolean = rng.gen_bool(0.5); // generates a random boolean with 50% probability
let c: char = rng.gen(); // generates a random unicode character
```

## Deep Dive

The `ThreadRng` type returned by `thread_rng` implements the `Rng` trait, which provides all the necessary methods for generating random values. This trait also allows us to specify the type of randomness we want, such as uniform or Gaussian distribution.
```
Rust
let num: f64 = rng.gen(); // generates a random float between 0 and 1
let num2: f64 = rng.gen_range(1.0..=5.0); // generates a random float between 1 and 5 (inclusive)
```
Additionally, the `rand` crate also provides support for generating random numbers from other distributions, such as the exponential or geometric distribution.
```
Rust
let num: f64 = rng.gen::<StandardNormal>();
let num2: f64 = rng.gen::<Exp>(0.5); // generates a random float from the exponential distribution with a rate of 0.5
```
This deeper dive into the `rand` crate highlights the versatility and flexibility of Rust when it comes to generating random numbers.

## See Also

For more information on generating random numbers in Rust, check out the official documentation for the `rand` crate here: 
- https://docs.rs/rand/0.8.4/rand/
- https://docs.rs/rand_distr/0.2.2/rand_distr/