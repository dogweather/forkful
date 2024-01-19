---
title:                "Generating random numbers"
html_title:           "Gleam recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Random Numbers Generation in Gleam

## What & Why?

Generating random numbers is about producing a series of numbers that lack any predictable pattern. Programmers do this when they need unpredictability in programs, like in gaming, cryptography, or simulations.

## How to:

In Gleam, you utilize the `rand` function from the `gleam/otp` library to generate random numbers. First, you will need to import the module:

```Gleam
import gleam/otp.{self, StartLink, Supervisor}
import gleam/otp/process.Signal.{self, Number2}
import gleam/otp/process.{self, ProcessBuilder, Tuple}
import gleam/int
```

Next, you can use the `rand` function within a simple `main` function like this:

```Gleam
pub fn main() -> Nil {
  let process_builder = process.spawn_link(fn(_) {
    let random_num = int.rand()
    process.send_message(process.parent(), Number2(random_num, random_num))
    Ok(Nil)
  })

  let pid = process_builder()
  let Number2(num1, num2) = process.receive_message(pid)

  io.println(int.to_string(num1))
  io.println(int.to_string(num2))
}
```

When the `main` function runs, it will print two identical random numbers.

## Deep Dive

Historical context-wise, random number generators (RNGs) go back centuries, to games of chance and lotteries. In programming, RNGs gained importance with the need for chaotic behavior in computer simulations and security algorithms.

For alternatives, there's pseudo-random numbers. These are technically deterministic, but within enormous periods, like the Mersenne Twister's 2^19937−1.

In Gleam, `int.rand()` uses Erlang's :rand module under the hood, meaning it's cryptographically secure and uniformly distributed RNG, using the algorithms of the OTP crypto library.

## See Also

To dive deeper into Gleam's offerings, check out the [Gleam documentation](https://gleam.run).

To understand more about the history and varieties of RNGs, see [Random Number Generation (Wikipedia)](https://en.wikipedia.org/wiki/Random_number_generation).

If want to know more about the underlying Erlang's :rand module, you can check [Erlang's RNG documentation](https://erlang.org/doc/man/rand.html).

Remember, the more you know, the more you can "gleam" out of your coding experience.