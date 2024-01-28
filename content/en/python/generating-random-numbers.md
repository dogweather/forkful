---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:16.869249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a common necessity in programming for tasks ranging from data analysis simulations to game development. It's critical in creating systems that rely on unpredictability or the simulation of real-world randomness.

## How to:

Python makes generating random numbers straightforward through the `random` module, which includes a variety of functions suited to different needs.

To generate a simple random float between 0.0 and 1.0:

```Python
import random

print(random.random())
```

For integers within a specific range:

```Python
print(random.randint(1, 10))  # Includes both 1 and 10
```

When you need a random element from a list:

```Python
items = ['apple', 'banana', 'cherry']
print(random.choice(items))
```

To shuffle a list in place, thereby randomizing its order:

```Python
random.shuffle(items)
print(items)
```

For cryptographic purposes or when you need more secure randomness (e.g., for generating a secure token), you'd use the `secrets` module instead:

```Python
import secrets

print(secrets.token_hex(16))  # Generates a secure 16-byte hex token
```

## Deep Dive

The `random` module in Python uses a pseudo-random number generator (PRNG) based on the Mersenne Twister algorithm, which is suitable for simulation and modeling but not for security-sensitive applications. The reason lies in its deterministic nature; given the same seed, the PRNG will produce the same sequence of numbers. For scenarios requiring unpredictability, such as in security applications or creating cryptographic keys, the `secrets` module, introduced in Python 3.6, provides access to a more secure source of randomness.

The `secrets` module is designed to be the go-to for generating cryptographic random numbers, offering a higher degree of randomness by utilizing sources provided by the operating system, which are often designed to accumulate environmental noise from device drivers and other sources to generate true randomness.

While the `random` module is sufficient and offers high performance for simulations, modeling, and casual use, programmers handling sensitive data or requiring cryptographic security should opt for `secrets` to ensure the confidentiality and integrity of their data.

## See also

### Official Documentation and References
- [Python `random` Module Documentation](https://docs.python.org/3/library/random.html)

### Comprehensive Tutorials
- **Real Python**
  - [Guide on Python's random Module](https://realpython.com/python-random/)
- **GeeksforGeeks**
  - [Python Random Module](https://www.geeksforgeeks.org/python-random-module/)

### Video Tutorials
- **Corey Schafer**
  - [Python Tutorial: Generate Random Numbers and Data Using the random Module](https://www.youtube.com/watch?v=KzqSDvzOFNA)
- **Tech With Tim**
  - [Python Quick Tip: The Difference Between “import random” and “from random import *”](https://www.youtube.com/watch?v=KzqSDvzOFNA)

### Advanced Usage and Techniques
- **Towards Data Science**
  - [How to Use the Random Module in Python](https://towardsdatascience.com/how-to-use-the-random-module-in-python-c7580b4e6e7e)
- **Stack Abuse**
  - [Generating Random Data in Python (Guide)](https://stackabuse.com/generating-random-data-in-python-guide/)

### Community Resources
- **Stack Overflow**
  - [How to generat a random number in Python](https://stackoverflow.com/questions/3996904/generate-random-integers-between-0-and-9)
