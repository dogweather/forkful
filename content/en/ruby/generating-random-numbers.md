---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers refers to producing a sequence of numbers that lacks any pattern. Developers often use random numbers in cryptography, algorithms, testing, or to add unpredictability to their programs.

## How to:

In Ruby, the simplest way to generate a random number is using the `rand` method. 

```Ruby
puts rand(100) # Random number between 0 and 99
```

For a range of numbers:

```Ruby
puts rand(1..10) # Random number between 1 and 10
```

To generate the same sequence of random numbers for debugging purposes, we use a seed value like so:

```Ruby
srand 12345 # Setting the seed
puts rand(100) # Returns 69 due to the seed
```

## Deep Dive

1. **Historical context**: Random number generation has a long history, with mechanical devices like dice and roulette wheels historically implementing randomness. In the computer age, random numbers are generated through algorithms, which are deterministic in nature. Thus, we refer to them as pseudorandom.

2. **Alternatives**: Apart from the `rand` method, the `Random` class in Ruby provides more options. We can create a random number object and call methods on it.

    ```Ruby
    rng = Random.new
    puts rng.rand(100) # Random number between 0 and 99
    ```

3. **Implementation details**: In Ruby, the default `rand` method without any argument returns a random floating-point number between 0.0 and 1.0. The generated numbers are less than 1 and vary based on the argument passed. If the argument is an integer, it returns an integer. If the argument is a range, it returns a number within that range.


## See Also 

1. [Random numbers in Ruby (Ruby-Doc official documentation)](https://ruby-doc.org/core-2.7.1/Random.html)
2. ['Mathn' in Ruby (Ruby-Doc official documentation)](https://ruby-doc.org/stdlib-2.7.0/libdoc/mathn/rdoc/Mathn.html)
3. [Wikipedia page on pseudorandom number generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator).