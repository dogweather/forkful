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

Generating random numbers is making numbers appear "out of nowhere," without a discernible pattern. We use randomness in programs for tasks like anonymizing data, running statistical simulations, or adding gameplay variety.

## How To

Python’s built-in `random` module makes this simple:

```Python
import random

# random float between 0 - 1
print(random.random())
```

Or get a random integer within a range:

```Python
import random

# random integer between 1 - 10
print(random.randint(1, 10))
```

## Deep Dive

Python's `random` module first appeared in version 1.1 (1994). It implements pseudo-random number generators (PRNGs), that generate seemingly random sequences from deterministic initial conditions, a "seed."

An alternative is Python's `secrets` module, which generates true random numbers, safe for cryptographic use.

We can even implement our own PRNG; Linear Congruential Generator (LCG) is the simplest:

```Python
def lcg(modulus, a, c, seed):
    """Linear congruential generator."""
    while True:
        seed = (a * seed + c) % modulus
        yield seed

gen = lcg(9, 2, 0, 1)
for _ in range(10):
    print(next(gen))
```
A bit complex, it's generally useful to stick with built-ins.

## See Also

- Official [Python `random` module documentation](https://docs.python.org/3/library/random.html)
- [Module `secrets` official documentation](https://docs.python.org/3/library/secrets.html)
- Wiki article on [Pseudo-random number generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- A tech post about [random vs. secrets in Python](https://yasoob.me/posts/understanding-more-about-python-random-and-secrets-module/)