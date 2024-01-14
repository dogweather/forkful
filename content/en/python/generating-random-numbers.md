---
title:    "Python recipe: Generating random numbers"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why
Have you ever played a game that involved rolling a dice or drawing a card at random? Did you know that the outcome of those random events can actually be simulated using Python? Generating random numbers is not only useful for creating games, but also for statistical analysis and cryptography.

## How To
To generate random numbers in Python, we first need to import the `random` module. Then, we can use the `randint()` function to generate a random integer between a specified range. For example:

```Python
import random
print(random.randint(1, 10))
```

This code will print a random integer between 1 and 10, inclusive. We can also use the `random` module to generate a random floating-point number using the `uniform()` function. For instance:

```Python
import random
print(random.uniform(0, 1))
```

This will print a random decimal number between 0 and 1. Additionally, we can use the `choice()` function to randomly select an element from a list. Check out the code snippet below:

```Python
import random
fruits = ['apple', 'banana', 'orange', 'mango']
print(random.choice(fruits))
```

This will print a random fruit from the list. As you can see, generating random numbers is quite easy in Python and can be done using just a few lines of code.

## Deep Dive
Behind the scenes, the `random` module uses a pseudorandom number generator to produce a sequence of numbers that appear random. This sequence is not truly random, as it is based on a starting value known as the seed. If the same seed is used, the same sequence of numbers will be generated. This can be useful for debugging purposes, as it allows us to reproduce the same "random" results.

If we want to generate truly random numbers in Python, we can use the `secrets` module. This module uses a cryptographically secure pseudorandom number generator, making the results much harder to predict. However, keep in mind that generating truly random numbers can be computationally expensive, so it should only be used when necessary.

## See Also
- [Python random module documentation](https://docs.python.org/3/library/random.html)
- [Real Python article on generating random numbers](https://realpython.com/python-random/)
- [Randomness in Python: A Closer Look](https://towardsdatascience.com/randomness-in-python-a-closer-look-7c937495d831)