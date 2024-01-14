---
title:                "Ruby recipe: Generating random numbers"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate random numbers for a game, simulation, or data analysis? The ability to generate random numbers is a fundamental concept in programming and can add an unpredictable element to your code. In this blog post, we will explore how to generate random numbers in Ruby and why it can be a useful skill to have in your programming toolkit.

## How To

To generate a random number in Ruby, we first need to require the `securerandom` library. This library provides a set of methods for generating random numbers, strings, and other data. We can then use the `random_number` method to generate a float between 0 and 1.

```Ruby
require 'securerandom'
puts SecureRandom.random_number
```

If we want to generate a random number within a specific range, we can use the `rand` method. This method takes two arguments, a minimum and maximum value, and will generate a random integer between those two values.

```Ruby
puts rand(1..10)  # Output: 8
puts rand(1...100)  # Output: 45
```

We can also generate a random string using the `base64` method, which will return a string containing random characters encoded in Base64.

```Ruby
puts SecureRandom.base64  # Output: "lLk6CFcHfXSIL0FcDlgeXg=="
```

## Deep Dive

Behind the scenes, Ruby uses a pseudorandom number generator (PRNG) to generate random numbers. A PRNG is an algorithm that produces a sequence of numbers that appear to be random but are actually determined by a starting point called the seed. By default, Ruby uses the operating system's time and process ID as the seed, but we can also specify a custom seed using the `srand` method.

```Ruby
srand 1234
puts rand(1..10)  # Output: 5
srand 1234
puts rand(1..10)  # Output: 5 (same as above)
```

It is important to note that PRNGs are not truly random, as they are deterministic and will produce the same sequence of numbers given the same seed. Additionally, if the seed is not changed, the sequence of numbers will eventually repeat itself. This is why it is recommended to use a different seed every time your program runs to ensure a more unpredictable pattern of random numbers.

## See Also

- Documentation for Ruby's `securerandom` library: https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/index.html
- A dive into the inner workings of Ruby's random number generator: https://medium.com/@6170/learning-to-love-psuedo-random-generation-18f1be6f63b8
- Creating a simple guessing game using random numbers in Ruby: https://www.theodinproject.com/courses/ruby-programming/lessons/guessing-game