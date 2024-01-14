---
title:                "Python recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
In Python, generating random numbers is a useful tool for a variety of tasks such as simulating events, generating test data, or creating games. It allows for unpredictability and randomness in the code, adding an element of surprise and realism.

## How To
To generate random numbers in Python, we will be using the built-in `random` module. First, we need to import the module at the top of our code:

```Python
import random
```

Next, we can use the `randint()` function to generate a random integer within a given range. For example, if we want to generate a random number between 1 to 10, we can use:

```Python
random.randint(1, 10)
```

This code will generate a random integer between 1 and 10 (inclusive). You can also use the `randrange()` function for more flexibility in defining the range of numbers. For example, to generate a random number between 1 and 100 in increments of 5, we can use:

```Python
random.randrange(1, 100, 5)
```

We can also generate random floats using the `uniform()` function. This function takes in two parameters, the lower and upper bound for the range of numbers. For example:

```Python
random.uniform(1, 10)
```

This code will generate a random float between 1 and 10. Finally, we can also generate random numbers from a given list using the `choice()` function. For example:

```Python
random.choice([1, 2, 3, 4, 5])
```

This code will generate a random number from the list provided.

## Deep Dive
Behind the scenes, the `random` module uses a pseudo-random number generator (PRNG) algorithm to generate random numbers. This algorithm takes in a starting point or "seed" and uses mathematical calculations to produce a sequence of numbers that appear to be random. However, it is important to note that these numbers are not truly random, as they are generated using a set algorithm and seed.

PRNG algorithms are designed to produce numbers that pass certain statistical tests for randomness, but they are not truly random. If the same seed is used, the same sequence of numbers will be generated each time. To avoid this, it is recommended to use a different seed each time the code is run or to use a more complex PRNG algorithm.

## See Also
- [Python random module documentation](https://docs.python.org/3/library/random.html)
- [Overview of Random Number Generators in Python](https://realpython.com/python-random/)
- [Understanding the Python random module](https://www.codementor.io/@irfanu/understanding-the-python-random-module-toptal-pajun1zq3)