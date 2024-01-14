---
title:    "Rust recipe: Generating random numbers"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a common task in programming, used for a variety of purposes such as creating random game scenarios, testing algorithms, and generating unique identifiers. In this blog post, we will explore how to generate random numbers in Rust and the various options available.

## How To

To generate random numbers in Rust, we will use the `rand` crate, which is a popular random number generation library in the Rust ecosystem. To use this crate, add the following line to your `Cargo.toml` file:

```Rust
[dependencies]
rand = "0.8.4"
```

Next, we will need to import the crate into our Rust code:

```Rust
use rand::Rng;
```

Now, let's dive into some examples of how to generate random numbers using `rand`:

1. Generate a random integer between 1 and 100:

```Rust
let mut rng = rand::thread_rng();
let random_number = rng.gen_range(1..=100);
```

2. Generate a random float between 0.0 and 1.0:

```Rust
let mut rng = rand::thread_rng();
let random_number = rng.gen::<f64>();
```

3. Generate a random boolean:

```Rust
let mut rng = rand::thread_rng();
let random_bool = rng.gen::<bool>();
```

The `thread_rng()` method creates a random number generator using a thread-local source of randomness. This ensures that each time we call the `gen()` or `gen_range()` method, we get a different random number. Next, we use the `gen()` method to generate a random number of the specified type.

## Deep Dive

The `rand` crate uses pseudorandom number generators (PRNGs) that generate numbers based on a mathematical algorithm. These numbers are deterministic, meaning that given the same starting point (or seed), the numbers will always be the same. This can be useful for testing or generating consistent results.

One commonly used PRNG in the `rand` crate is the Mersenne Twister algorithm (MT19937). This algorithm is known for its high-quality randomness and speed, making it an excellent choice for generating random numbers.

It is important to note that PRNGs are not cryptographically secure, meaning that they should not be used for encryption or securing sensitive data. For these purposes, a cryptographically secure random number generator should be used.

## See Also

- Official `rand` crate documentation: https://docs.rs/rand
- Rust Book chapter on random numbers: https://doc.rust-lang.org/book/ch07-06-sex-and-randomness.html
- Random number generation in other languages: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random