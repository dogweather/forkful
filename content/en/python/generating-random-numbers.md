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

## Why
Random numbers play an important role in various aspects of computer programming, from simulations to cryptography. Generating random numbers can help add an element of unpredictability and randomness to your code, making it more versatile and dynamic.

## How To

To generate random numbers in Python, we can use the built-in `random` module. First, we need to import the module into our code:

```Python
import random
```

### Generating a Single Random Number
To generate a single random number within a specified range, we can use the `random.randint()` function, which takes in two arguments - the lower and upper bounds. For example, to generate a random number between 1 and 10:

```Python
num = random.randint(1, 10)
print(num)
# Output: A random number between 1 and 10
```

### Generating a List of Random Numbers
We can also use the `random` module to generate a list of random numbers. For this, we can use the `random.sample()` function, which takes in three arguments - a range, the number of elements to be selected, and whether the elements can be repeated or not.

```Python
# List of 5 unique random numbers between 1 and 50
rand_list = random.sample(range(1, 50), 5)
print(rand_list)
# Output: [32, 12, 49, 5, 26]
```

### Setting a Seed
Sometimes we may want to generate the same set of random numbers every time our code is run. We can achieve this by setting a seed using the `random.seed()` function. This ensures that the sequence of random numbers generated is the same every time the program is executed.

```Python
random.seed(42)
print(random.randint(1, 10))
# Output: 9

random.seed(42)
print(random.randint(1, 10))
# Output: 9
```

## Deep Dive
Random numbers are generated using a mathematical algorithm that produces seemingly unpredictable results. However, these results are not truly random, as they are based on a starting value called a seed. This seed helps determine the sequence of numbers that will be generated. By using different seeds, we can generate different sequences of random numbers. 

It is important to note that the results of a random number generation algorithm must pass certain statistical tests to ensure that they are truly unpredictable. The `random` module in Python uses the `Mersenne Twister` algorithm, which has been deemed to be a high-quality generator of random numbers. 

## See Also
- Official `random` module documentation: https://docs.python.org/3/library/random.html
- Using Random Number in Python: https://realpython.com/python-random/