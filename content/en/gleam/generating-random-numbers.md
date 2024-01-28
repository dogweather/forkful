---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:11.646203-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in programming is a fundamental operation used in a wide range of applications, from simulations to security algorithms. In Gleam, a statically typed language for the Erlang VM, this task involves leveraging Erlang libraries and functions due to Gleam's strong interoperability features.

## How to:

Gleam makes it relatively straightforward to generate random numbers by utilizing Erlang's built-in random number generator. You'll primarily interface with the `rand` module. Here’s a basic example showing how to generate a random integer within a specified range:

```gleam
import gleam/erlang
import gleam/int

pub fn generate_random_between(low: Int, high: Int) -> Int {
  erlang.apply(
    module: "rand",
    function: "uniform",
    arguments: [int.to_float(high - low + 1)]
  )
  |> erlang.to_float
  |> float.floor
  |> float.to_int
  |> Result.unwrap
  |> int.add(low)
}
```

To use the function, simply call it with the desired range:

```gleam
pub fn main() {
  let random_number = generate_random_between(1, 10)
  erlang.display(random_number)
}
```

Sample output for multiple runs might look like this (though actual results will vary):

```
3
7
1
9
```

## Deep Dive

The approach to generating random numbers in Gleam largely depends on the Erlang environment, given Gleam's nature as a language that compiles to Erlang bytecode. Historically, Erlang's random number generation capabilities have evolved, with the `rand` module being a relatively recent and more robust addition, replacing the older `random` module due to its improved algorithms and features.

Gleam's method of interfacing with Erlang for tasks such as generating random numbers illustrates a key advantage of the language: the ability to leverage the mature ecosystem of Erlang/OTP while providing a type-safe, functional alternative to Erlang itself. It's worth noting, however, that for cryptographic purposes, developers should consider using specifically designed libraries or modules that guarantee cryptographic security. The `rand` module is suitable for most purposes that don't require cryptographic-grade randomness but exploring libraries in the Erlang ecosystem dedicated to security might offer better alternatives for those specific needs.

## See also

### Gleam Official Documentation
- [Gleam StdLib Documentation: Random](https://hexdocs.pm/gleam_stdlib/gleam/random/)

### Blog Posts and Tutorials
- **The Gleam Book**: [Working with Random Values](https://gleam.run/book/tour/random.html)
- **Medium Articles**: [Introduction to Gleam for Erlang Developers](https://medium.com/@lpil/introducing-gleam-8e9e1400ed9a)

### Community Resources and Examples
- [Gleam GitHub Examples](https://github.com/gleam-lang/examples)
- **Erlang Forums**: [Gleam Language Section](https://erlangforums.com/c/gleam/23)
