---
title:                "Ruby recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed a random number in your code? Maybe for a game, a lottery simulation, or even for password generation? Learning how to generate random numbers in Ruby can be a useful skill for any programmer.

## How To

Generating random numbers in Ruby is actually quite simple. All you need is the `rand` method. Let's take a look at an example:

```Ruby
# Generate a random number between 1 and 10
random_number = rand(1..10)
puts random_number
```

In this code, we use the `rand` method with an argument of `1..10` to specify the range of numbers we want the random number to be generated from. You can also use `rand(number)` to generate a number between 0 and that number. For example:

```Ruby
# Generate a random number between 0 and 100
random_number = rand(100)
puts random_number
```

Another useful method is `rand(n..m)`, which generates a number between the given range, including both `n` and `m`. Here's an example:

```Ruby
# Generate a random number between 5 and 15
random_number = rand(5..15)
puts random_number
```

You can also use the `rand` method with arrays to generate a random element from that array. For example:

```Ruby
# Generate a random superhero name from an array
superhero_names = ["Superman", "Batman", "Spiderman", "Wonder Woman", "Captain America"]
random_name = superhero_names[rand(superhero_names.length)]
puts random_name
```

This will output a random superhero name from the array.

## Deep Dive

Behind the scenes, the `rand` method uses a pseudo-random number generator algorithm, which generates a sequence of seemingly random numbers based on a given seed. The seed is used as a starting point for the algorithm, and if the same seed is used, the same sequence of numbers will be generated.

To get a different sequence of numbers every time, Ruby uses the current time in milliseconds as the default seed for the `rand` method. However, you can also set a specific seed if you want to get the same sequence of numbers every time. For example:

```Ruby
# Set a specific seed to get the same sequence of numbers
srand(1234)
puts rand(100)
puts rand(100)
puts rand(100)
```

This will output the numbers 62, 1, and 28, in that order, every time it's run.

## See Also

- Ruby `Random` class: https://ruby-doc.org/core-2.7.2/Random.html
- Ruby `Kernel` module: https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-rand
- Pseudo-random number generator: https://en.wikipedia.org/wiki/Pseudorandom_number_generator