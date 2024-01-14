---
title:                "Python recipe: Generating random numbers"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Random numbers are an essential part of many programming tasks, from creating simulations to shuffling data. By generating random numbers, we can add an element of unpredictability to our programs, making them more realistic and versatile.

## How To
To generate random numbers in Python, we can use the built-in `random` module. First, we need to import the module:

```Python
import random
```

#### Generating a single random number
To generate a single random number, we can use the `random.random()` function, which returns a floating-point value between 0 and 1.

```Python
random_number = random.random()
print(random_number)
```

Output:
```
0.4758931456
```

#### Generating random integers
If we need to generate random integers within a specific range, we can use the `random.randint()` function, which takes two parameters, the lower and upper bounds of the range.

```Python
random_integer = random.randint(1, 10)
print(random_integer)
```

Output:
```
6
```

#### Selecting random elements from a list
We can also use the `random.choice()` function to select a random element from a list.

```Python
list_of_colors = ["red", "green", "blue"]
random_color = random.choice(list_of_colors)
print(random_color)
```

Output:
```
blue
```

## Deep Dive
Behind the scenes, the `random` module uses a mathematical algorithm called the Mersenne Twister to generate random numbers. This algorithm uses a seed value to generate a sequence of pseudo-random numbers. We can set our own seed value using the `random.seed()` function, which allows us to get the same sequence of random numbers each time we run the code.

```Python
random.seed(123)
print(random.random())
print(random.random())
```

Output:
```
0.5006850621447527
0.8431200220727286
```

It is important to note that the `random` module is not truly random, as it relies on a deterministic algorithm. However, for most practical purposes, the generated numbers are sufficiently unpredictable.

## See Also
- [Python documentation on the random module](https://docs.python.org/3/library/random.html)
- [Blog post on generating random numbers in Python](https://realpython.com/python-random/)
- [Explanation of the Mersenne Twister algorithm](https://en.wikipedia.org/wiki/Mersenne_Twister)