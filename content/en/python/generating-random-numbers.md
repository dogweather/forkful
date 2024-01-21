---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:48.456913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random numbers are unpredictable values. Programmers use them for simulations, testing, games, and anytime they need a dose of unpredictability in their code.

## How to:
Python makes it easy to generate random numbers with the `random` module. Here's a quick look at how you can roll the dice:

```Python
import random

# Random float: 0.0 <= number < 1.0
print(random.random())

# Random float: 5 <= number < 10
print(random.uniform(5, 10))

# Random integer: 1 to 6, inclusive
print(random.randint(1, 6))

# Random element from a list
print(random.choice(['apple', 'banana', 'cherry', 'date']))

# Random multiple elements from a list without replacement
print(random.sample(['apple', 'banana', 'cherry', 'date'], 2))
```

Output might look like:
```
0.435667762349
7.28295613718
4
'cherry'
['banana', 'date']
```
Remember: Running it yourself will give different results, because, well, they're random.

## Deep Dive
The first thing to know is that random numbers in programming are not truly random. They're "pseudo-random", which means a mathematical algorithm generates them. The `random` module in Python uses the Mersenne Twister, a popular algorithm known for its high-quality randomness.

Historically, generating random numbers mechanically was a challenge. People used dice, shuffled cards, or complex machines. When computers entered the picture, they needed a reliable way to simulate randomness. That's where the algorithms come in.

There are alternatives to `random`, like the `secrets` module in Python for cryptographic purposes. Why? Because `random` isn't safe for encryption and the like—predictability isn't great for security.

As for implementation, when using `random`, you can set a 'seed' to produce the same sequence of numbers every time. Useful for debugging, not so much when you need different results each run.

## See Also
- Python `random` module docs: https://docs.python.org/3/library/random.html
- An explanation of pseudo-randomness: https://www.random.org/randomness/
- Python `secrets` module docs for secure random numbers: https://docs.python.org/3/library/secrets.html