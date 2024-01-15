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

## Why
Generating random numbers is a fundamental task in programming that allows us to introduce unpredictability and variety into our code. Whether you need to create a random password or simulate a game, the ability to generate random numbers is a valuable skill for any programmer to have in their toolkit.

## How To
Generating random numbers in Gleam is a straightforward process that can be achieved using the `Random` module. Let's take a look at a simple example of generating a random number between 1 and 10:

```
Gleam import Random

let random_number = Random.int(1,10)
```

In the code above, we first import the `Random` module and then use the `int` function to generate a random number between 1 and 10. We can also use the `float` function to generate a random floating-point number between 0 and 1. 

If we want to generate a random number within a specific range, we can use the `Range` module. Here's an example of generating a random number between 50 and 100 using the `Range` module:

```
Gleam import Random
Gleam import Range

let range = Range.new(50,100)
let random_number = Random.int_in_range(range)
```

We first create a `Range` object using the `new` function and then use the `int_in_range` function to generate a random number within that range.

## Deep Dive
When it comes to generating random numbers, it's important to understand that computers can't truly generate randomness. They use algorithms and seed values to create a sequence of numbers that appear random to us. This means that if we use the same seed value, we will get the same sequence of random numbers every time.

To avoid this, it's recommended to use a different seed value each time you generate a random number. You can do this using the `Random.seed` function. Here's an example:

```
Gleam import Random

let random_number = Random.int(1,10, Random.seed())
```

In the example above, we are using the `seed` function as the third argument of the `int` function. This will generate a new seed value each time we call the `int` function, giving us a different sequence of random numbers.

It's also worth noting that the default seed value for the `Random` module is based on the current time. This means that if you call the `Random` module multiple times within a short period, you may get the same sequence of random numbers. To avoid this, you can explicitly set the seed value using the `set_seed` function.

## See Also
- Official Gleam documentation on `Random` module: https://gleam.run/modules/random/
- Documentation on the `Range` module: https://gleam.run/modules/range/
- A deep-dive article on random number generation in Gleam: https://thewiredfox.co.uk/blog/generating-random-numbers-in-gleam