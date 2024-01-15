---
title:                "Generating random numbers"
html_title:           "Ruby recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers play a crucial role in many applications, from games to statistical analysis. In Ruby, generating random numbers is a useful skill to have in your coding arsenal. Whether you need to simulate dice rolls or generate a randomized dataset, understanding how to generate random numbers can be beneficial in various scenarios.

## How To

Generating random numbers in Ruby can be achieved using the `rand` method. Let's take a look at how this method works:

```Ruby
# To generate a random float between 0 and 1
rand # Output: 0.346107832 

# To generate a random integer between 0 and n (inclusive)
rand(n) # Output: 7

# To generate a random float between m and n
rand(m..n) # Output: 5.825484193 
```

You can also pass in a range to the `rand` method to generate a random number within that range. Keep in mind that the generated number will always be less than the upper bound of the range.

```Ruby
# To generate a random number between 200 and 500
rand(200..500) # Output: 342 
```

To generate a random number with a decimal point, we can use `Float` to specify the precision:

```Ruby
# To generate a random float with 2 decimal places
Float(rand(0..9)).round(2) # Output: 3.85
```

## Deep Dive

The `rand` method uses a pseudo-random number generator (PRNG) to generate random numbers. A PRNG is an algorithm that produces a sequence of numbers that appear to be random, but in reality, they are deterministic. This means that the generated sequence of numbers is predictable, given the initial state of the PRNG.

In Ruby, the PRNG is seeded with the current time by default. This means that if you call the `rand` method multiple times within a short timeframe, you might get the same sequence of numbers. To avoid this, we can use the `srand` method to set a seed before calling the `rand` method. This will ensure that the generated sequence of numbers is different every time the code is executed.

```Ruby
# To generate a random number using a specific seed
srand(12345) # Set the seed to 12345
rand(10) # Output: 2
rand(10) # Output: 5
rand(10) # Output: 9
```

## See Also

- [Ruby Docs: Random](https://ruby-doc.org/core-3.0.2/Random.html)
- [Ruby Guides: Random Numbers](https://www.rubyguides.com/2019/11/ruby-random/)
- [Official Blog: The Basics of Random Number Generation](https://www.ruby-lang.org/en/news/2016/03/19/the-basics-of-random-number-generation-in-ruby/)