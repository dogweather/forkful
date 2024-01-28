---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:16.631335-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Ruby involves creating values that are unpredictable and vary each time your code runs. Programmers use random numbers for a variety of reasons, such as implementing logic in games, simulating experiments, or selecting random items from a list.

## How to:

Ruby makes it quite straightforward to generate random numbers using the `rand` method and the `Random` class. Here are a few examples:

To generate a random number between 0 and 1 (excluding 1):

```Ruby
puts rand
```

To get a random integer up to a specified value, for example, between 0 and 99:

```Ruby
puts rand(100)
```

If you need a number in a specific range, Ruby simplifies this too:

```Ruby
puts rand(1..10)
```

To ensure reproducibility, especially useful in testing, you can use the `Random` class with a seed:

```Ruby
rng = Random.new(1234)
puts rng.rand
puts rng.rand(100)
```

Sample output might look like this, though the first and second calls to `rand` without arguments will vary each time you run the code:

```Ruby
0.1915194503788923
22
7
0.1915194503788923
22
```

## Deep Dive

Ruby's approach to generating random numbers has evolved, but the core functionality has been consistent, relying heavily on underlying system libraries to ensure good randomness. Before the introduction of the `Random` class in Ruby 1.9, the `Kernel#rand` method was the primary tool for generating random numbers. The introduction of the `Random` class allowed for more flexibility, including the ability to create instances with their own seed values, making it easier to reproduce sequences of random numbers—a critical feature for debugging and testing.

Internally, Ruby uses a pseudo-random number generator (PRNG), which means the numbers are not truly random in the sense of being unpredictable by mathematics alone; they are deterministic but start from an unpredictable seed (usually based on the system clock). For most applications, this level of randomness is sufficient. However, for cryptographic purposes, Ruby's PRNG is not suitable as it is not designed to withstand the rigorous requirements for cryptographically secure random number generation.

For applications needing cryptographic security, Rubyists should look towards the `SecureRandom` module, part of the standard library, which provides methods to generate random numbers, strings, and hexadecimal values suitable for security-sensitive applications. `SecureRandom` sources randomness from the operating system's random number generator, making it a better choice for situations where the predictability of PRNG could be a liability.

## See also

### Official Ruby Documentation
- [Ruby Random Class](https://ruby-doc.org/core-3.1.2/Random.html)

### Tutorials and Guides
- **RubyGuides**: [How to Generate Random Numbers in Ruby](https://www.rubyguides.com/2018/05/random-numbers-in-ruby/)
- **Stack Overflow Tips**: [Generating a Random Number in Ruby](https://stackoverflow.com/questions/198460/how-to-get-a-random-number-in-ruby)
