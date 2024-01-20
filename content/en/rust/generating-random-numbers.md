---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# An Introduction On Generating Random Numbers in Rust

## What & Why?

Generating random numbers, put simply, is producing unpredictable values that typically fall within set constraints. Programmers need this for tasks like creating unique IDs, shuffling playlists, or simulating unpredictability in games.

## How to:

In Rust, `rand::Rng` trait is usually used to generate random numbers:

```Rust   
 use rand::Rng;

 fn main() {
    let mut rng = rand::thread_rng();
 
    let random_integer = rng.gen_range(1..101);
    println!("Random Integer: {}", random_integer);

    let random_float = rng.gen::<f64>();
    println!("Random Float: {:?}", random_float);
 }
```

Running the above code will give us output similar to this:

```   
Random Integer: 42
Random Float: 0.12345678
```

## Deep Dive

The Rust language adopted inspiration from the RNG implementations seen in libraries such as PCG and Xorshift. Rust currently uses two types of basic RNGs: `ThreadRng`, which is local to each thread and `SmallRng`, which is a small, fast RNG.

Alternatives to `rand::Rng` include other RNG libraries like `rand_pcg` or `rand_xoshiro`, or using system-specific ways of generating random numbers.

One thing to note is that, in Rust, the value range in `.gen_range()` is inclusive at the lower bound, and exclusive at the upper bound. So, `.gen_range(1..101)` generates a value between 1 and 100.

## See Also:

- [The Rust `rand` crate](https://docs.rs/rand): For documentation and further exploration.
- [`rand::Rng` trait documentation](https://docs.rs/rand/0.8.4/rand/trait.Rng.html): Specifics about Rng trait.
- [Wikipedia page on RNGs](https://en.wikipedia.org/wiki/Random_number_generation): Offers history and types of RNGs.

Knowledge of generating random numbers in Rust is a practical tool for your programmer toolkit. Happy coding!