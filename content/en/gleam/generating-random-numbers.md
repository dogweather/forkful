---
title:                "Gleam recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Why would anyone want to generate random numbers in a programming language? Well, there are plenty of use cases for random numbers, such as creating randomized data for testing, generating unique identifiers, or creating games with unpredictable elements. No matter the reason, learning how to generate random numbers in Gleam can come in handy for many programming projects.

## How To
Generating random numbers in Gleam is a straightforward and simple process. First, import the `random` module in your code. Then, use the `uniform` function to generate a random number within a given range. Let's see an example:

```Gleam
import random

random_number = random.uniform(1, 10)
```

In this code, we import the `random` module and use the `uniform` function to generate a random number between 1 and 10. You can change the range to fit your specific needs. Running this code will produce a different random number every time.

If you want to generate a random integer instead, you can use the `int_uniform` function. This function takes two arguments: the lower bound (inclusive) and the upper bound (exclusive). Let's see an example:

```Gleam
import random

random_integer = random.int_uniform(1, 10)
```

This code will produce a random integer between 1 and 9.

## Deep Dive
While the `uniform` and `int_uniform` functions are sufficient for most use cases, Gleam also provides a `normal` function for generating random numbers that follow a normal (Gaussian) distribution. This function takes two arguments: the mean and the standard deviation. Let's see an example:

```Gleam
import random

random_number = random.normal(5, 2.5)
```

In this code, we generate a random number with a mean of 5 and a standard deviation of 2.5. This means that the majority of the generated numbers will be around 5, with some outliers on both sides.

It's worth noting that the `random` module uses a random number generator that is seeded with the current time by default. This means that if you want to get the same sequence of random numbers every time you run your code, you can use the `reseed` function to set a specific seed. For example:

```Gleam
import random

random.reseed(1234)
```

This code will set the seed to 1234, so every time you run it, the sequence of random numbers will be the same.

## See Also
- [Gleam documentation on the `random` module](https://gleam.run/modules/random/)
- [Wikipedia page on pseudorandom number generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Article on generating random numbers in other programming languages](https://www.baeldung.com/java-generate-random-long-float-integer-double)