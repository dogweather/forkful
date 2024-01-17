---
title:                "Generating random numbers"
html_title:           "Python recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers refers to the process of generating a sequence of numbers that appear to be random. Programmers use this technique for a variety of reasons, including creating randomized data for testing or simulating real-world scenarios in their code.

## How to:
```Python
import random

# Generate a random integer between 1 and 10
random_num = random.randint(1, 10)

# Generate a random floating point number between 0 and 1
random_float = random.random()

# Generate a random number from a given range
random_range = random.randrange(5, 20, 3)

# Print the results
print(random_num)
print(random_float)
print(random_range)
```

Output:
3
0.758904815
11

## Deep Dive:
Generating random numbers has been a topic of interest for centuries, with the first recorded use dating back to ancient China. In modern times, computer programs are used to generate random numbers, with many languages, including Python, offering built-in functions for this purpose.

One alternative to using a built-in function is to use a pseudorandom number generator. These generators use an algorithm to produce a sequence of numbers that appear to be random, but are actually deterministic. This means that the same sequence of numbers will be generated each time, unless the seed is changed.

Implementations of random number generators can differ, with some focusing on producing numbers that pass statistical tests for randomness, while others prioritize speed and simplicity.

## See Also:
- [Python documentation for random module](https://docs.python.org/3/library/random.html)
- [Wikipedia article on random number generation](https://en.wikipedia.org/wiki/Random_number_generation)