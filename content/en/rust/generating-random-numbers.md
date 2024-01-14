---
title:                "Rust recipe: Generating random numbers"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers are a crucial component in many programming tasks such as generating encryption keys, creating random game worlds, and selecting items in a random order. In this post, we will explore the basics of generating random numbers in Rust and how to use this functionality in your own projects.

## How To

To generate random numbers in Rust, we first need to add the `rand` crate to our `Cargo.toml` file:

```Rust
[dependencies]
rand = "0.7.3"
```

Next, we need to import the `Rng` trait and the `thread_rng()` function from the `rand` crate:

```Rust
use rand::Rng;
use rand::thread_rng;
```

Now, we can use the `thread_rng()` function to create a random number generator and then call methods from the `Rng` trait to generate random numbers. For example, to generate a random integer between 1 and 100, we can use the `gen_range()` method:

```Rust
let mut rng = thread_rng();
let number = rng.gen_range(1, 101);
println!("Random number: {}", number);
```

Running this code will output a different random number each time it is executed.

## Deep Dive

Behind the scenes, the `rand` crate uses a pseudo-random number generator (PRNG) to generate its random numbers. This PRNG is seeded by the operating system's entropy source, ensuring a fairly random distribution. However, it is important to note that these numbers are not truly random and should not be used for cryptographic purposes.

Additionally, the `rand` crate offers various methods for generating different types of random numbers, such as floats, booleans, and even randomly shuffling items in a vector.

## See Also

If you want to learn more about generating random numbers in Rust, here are some helpful resources:

- [The Rust Book: Randomness](https://doc.rust-lang.org/book/ch07-05-separating-modules-into-different-files.html)
- [The rand crate documentation](https://docs.rs/rand/0.7.3/rand/)
- [The Rust Cookbook: Generating Random Numbers](https://rust-lang-nursery.github.io/rust-cookbook/science/mathematics/rand.html)