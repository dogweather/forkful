---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:50:01.133257-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Generating random numbers in Ruby is as simple as calling a method. Programmers do it for tasks like simulations, gaming, security, and testing.

## How to: (Як це зробити:)
```Ruby
# Basic random number
rand_num = rand(100) # Random number between 0 and 99
puts rand_num

# Random number within a range
range_num = rand(1..10) # Random number between 1 and 10
puts range_num

# Secure random number for cryptography
require 'securerandom'
secure_num = SecureRandom.random_number(100) # Random number between 0 and 99
puts secure_num
```
Sample output could look like:
```
42
7
53
```

## Deep Dive (Поглиблений Аналіз):
Random number generation in Ruby relies on pseudorandom number generators (PRNG), which simulate randomness algorithmically. Historically, PRNG methods have evolved to offer better randomness and avoid patterns.

Alternatives to Ruby's built-in `rand` method include the `Random` class for more control, or external libraries like SecureRandom for cryptographic purposes.

Implementation-wise, Ruby's `rand` pulls numbers from a PRNG that's seeded by default, or you can provide your own seed to replicate results.

## See Also (Дивись Також):
- Ruby's `Random` class documentation: [https://ruby-doc.org/core-3.1.0/Random.html](https://ruby-doc.org/core-3.1.0/Random.html)
- SecureRandom library for Ruby: [https://ruby-doc.org/stdlib-3.1.0/libdoc/securerandom/rdoc/SecureRandom.html](https://ruby-doc.org/stdlib-3.1.0/libdoc/securerandom/rdoc/SecureRandom.html)
- About pseudorandom number generators: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)