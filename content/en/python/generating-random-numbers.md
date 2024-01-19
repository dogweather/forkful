---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

In computer programming, generating random numbers is about producing a sequence of numbers that lack any pattern. As programmers, we use such randomness to create things like unique session identifiers, in games, simulations and cryptography.

## How to:

In Python, you can use the random module to generate random numbers. Here's how:

```python
import random
print(random.randint(1, 100))  # Prints a random integer between 1 and 100.
```

Here's another handy function to generate a random float:

```python
print(random.random())  # Prints a random float between 0 and 1.
```

And you might get outputs like this:

```python
52
0.345834234
```

You can also generate a list of unique random numbers:

```python
print(random.sample(range(100), 10))    # Prints 10 unique random numbers from 0 to 99
```

## Deep Dive

The concept of generating random numbers dates back to ancient times, with dice games being one of the earliest examples. But in the world of computers, "random" is a bit trickier. 

Here's why: Computers are deterministic machines, made to consistently repeat processes. Asking for randomness from a system built on predictability seems paradoxical. That's why we have pseudorandom numbers in computing. Generated using algorithms with deterministic behavior, they're not truly random but "random enough" for most purposes.

The main alternative to Python's random module is the numpy module, which provides the function `numpy.random()`. It's much faster for large arrays but requires an extra installation step.

Underneath the hood, Python's random number generator uses the Mersenne Twister algorithm, a widely-used, well-vetted pseudorandom generator.

## See Also

For more details and functionalities, check out Python's [official documentation](https://docs.python.org/3/library/random.html) for the random module, and [numpy's random sampling](https://numpy.org/doc/stable/reference/random/index.html) for large arrays.