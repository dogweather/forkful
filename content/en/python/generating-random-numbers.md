---
title:    "Python recipe: Generating random numbers"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

If you're new to programming, you might be wondering why someone would want to generate random numbers. Generating random numbers is a common task in many programming projects, ranging from games to statistical simulations. By adding an element of randomness, you can make your programs more unpredictable and interesting.

## How To

Generating random numbers in Python is a simple and straightforward process. The first step is to import the "random" module, which contains functions for generating random numbers. Then, you can use the "randint()" function to generate an integer between a given range. For example, to generate a random number between 1 and 10, we would use the following code:

```Python
import random
print(random.randint(1, 10))
```

The output of this code could be any number between 1 and 10, such as 5, 9, or 2. You can also use the "uniform()" function to generate a decimal number between a given range, or the "random()" function to generate a floating-point number between 0 and 1.

Besides these basic functions, the "random" module also offers more advanced options for random number generation. For example, you can use the "shuffle()" function to randomly shuffle a list, or the "choice()" function to select a random item from a list. You can even use the "sample()" function to randomly select multiple items from a list without repeating them.

## Deep Dive

Behind the scenes, Python uses a pseudorandom number generator (PRNG) to generate random numbers. This means that the numbers are not truly random, but rather they are generated through a deterministic algorithm. In order to produce diverse and unpredictable results, the PRNG uses a seed value that can be set manually or generated automatically.

It's important to note that the results of a PRNG can be replicated by setting the same seed value. This can be useful for testing purposes, but it can also be a security concern if the seed value is not kept confidential. To avoid this, you can use the "urandom" module to generate random numbers based on a cryptographically secure source of randomness.

## See Also

Here are some resources for further reading about generating random numbers in Python:

- [Official Python Documentation on Random](https://docs.python.org/3/library/random.html)
- [Real Python's Tutorial on Random Numbers in Python](https://realpython.com/python-random/)
- [GeeksforGeeks' Article on Generate Random Numbers in Python](https://www.geeksforgeeks.org/generating-random-number-list-in-python/)