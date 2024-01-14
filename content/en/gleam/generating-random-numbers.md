---
title:    "Gleam recipe: Generating random numbers"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers are an important concept in programming as they allow for unpredictability and randomness in our applications. This can be useful for tasks such as generating unique IDs, shuffling data, or creating randomized user experiences. In Gleam, there are multiple ways to generate random numbers that suit various use cases.

## How To

To generate random numbers in Gleam, we can use the `random` module that comes pre-installed with the language. This module provides functions for generating different types of random numbers, such as integers, floats, and booleans.

Let's see an example of generating a random integer between 1 and 10:

```Gleam
import random

let random_number = random.int(1, 10)
```

We can also use the `float` function to generate a random float between 0 and 1:

```Gleam
import random

let random_float = random.float(0.0, 1.0)
```

And for a random boolean value, we can use the `bool` function:

```Gleam
import random

let random_bool = random.bool()
```

In addition, the `random` module also allows us to set a seed for our random number generation by using the `seed` function. This can be useful for creating a consistent set of random numbers that can be reproduced.

```Gleam
import random

let random_number = random.int(1, 10, seed=1234)
```

## Deep Dive

Under the hood, Gleam's random number generation is based on the Random number generator (RNG) from the standard library. This RNG uses the Xorshift algorithm, which is known for its high-performance and good randomness properties. The `random` module in Gleam also supports the use of user-defined RNGs, allowing for more customization and control over the generated random numbers.

It is also worth noting that the `random` module is not thread-safe, so it should not be used for generating random numbers in parallel contexts. Instead, the `random` module can be used to generate a large enough random number sequence and then be shared among different threads.

## See Also

- Official Gleam Documentation on Random Numbers: https://gleam.run/documentation/stdlib/random/
- The Xorshift Algorithm: https://en.wikipedia.org/wiki/Xorshift
- Thread-safety in Programming: https://en.wikipedia.org/wiki/Thread_safety