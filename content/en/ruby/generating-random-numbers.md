---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:41.340808-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are unpredictable values. Programmers use them for tasks like simulating events, creating test data, and building games.

## How to:
### Generate a Random Number Within a Range
```Ruby
rand(1..10) # Generates a number between 1 and 10
```
Sample output: `5`

### Generate a Random Float Between 0 and 1
```Ruby
rand # Implicitly between 0 and 1
```
Sample output: `0.4372022471910112`

### Generate a Random Number Without a Set Upper Bound
```Ruby
rand(100) # Generates a number between 0 and 99
```
Sample output: `42`

## Deep Dive
Random number generation in Ruby uses objects of class `Random` under the hood, which implements Mersenne Twister—known for its speed and randomness quality. Before this, older algorithms like Linear Congruential Generators were common, offering speed but imperfect randomness. While `rand` is fine for casual use, for cryptography, you'll want Ruby's `SecureRandom` which is designed to be more secure and less predictable.

There's a quirk in Ruby (and many other languages)—"random" numbers are pseudo-random unless the generator is seeded with some external entropy, like system timing or mouse movements. This means that if you set the same seed, you get the same sequence.

Alternatives to `rand` include libraries like `faker` for more specific or complex needs, like generating names, addresses, or even random but valid data structures.

## See Also
- Ruby's own documentation on the Random class: [Random - Ruby-Doc.org](https://ruby-doc.org/core-3.1.2/Random.html)
- The `SecureRandom` library for security-sensitive randomness: [SecureRandom - Ruby-Doc.org](https://ruby-doc.org/stdlib-3.1.2/libdoc/securerandom/rdoc/SecureRandom.html)
- The `faker` gem for generating a wide variety of random test data: [Faker - RubyGems.org](https://rubygems.org/gems/faker)