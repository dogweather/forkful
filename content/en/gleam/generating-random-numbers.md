---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers means creating a number that can't be reasonably predicted. Programmers utilize this for a variety of purposes like creating unique session IDs, building randomized algorithms, and adding unpredictability in games.

## How to:

Here's how you generate random numbers in Gleam:

```Gleam
import gleam/random.{int}

pub fn dice() {
  let roll = random.int(1, 6)
  case roll {
    Ok(value) -> io.println(value)
    Error(Nil) -> io.println("Something went wrong!")
  }
}
```

Running `dice()` a few times will give you random numbers between 1 and 6.

## Deep Dive

Historically, true randomness is impossible in computers, hence we use pseudorandom number generators, simulating randomness. 

Alternatives to the `random.int` function in Gleam include the `random.float` function for random floating-point numbers, and `random.bits` for a series of random bits.

Gleam's `random.int` function uses the OS-provided random number generator to avoid predictable number sequences, enhancing the security of the random numbers generated. 

## See Also

For more information on random numbers in Gleam, check out the Gleam documentation at https://hexdocs.pm/gleam_stdlib/gleam/random.html. For broader understanding, refer to the Gleam Language website at https://gleam.run/.