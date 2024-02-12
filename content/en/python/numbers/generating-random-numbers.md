---
title:                "Generating random numbers"
aliases: - /en/python/generating-random-numbers.md
date:                  2024-01-27T20:26:11.845598-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers involves creating numbers that cannot be reasonably predicted better than by chance, which is essential for developing simulations, games, and security algorithms. Programmers do this to introduce unpredictability or simulate real-world phenomena in their applications.

## How to:

Python provides the `random` module that helps in generating random numbers for various uses. Here's how to get started:

1. **Importing the module**
    ```Python
    import random
    ```

2. **Generating a Random Integer**
    Between any two numbers.
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    Sample output: `7`

3. **Generating a Float**
    Between 0 and 1.
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    Sample output: `0.436432634653`

    If you need a float in a different range, multiply:
    ```Python
    random_float_range = random.random() * 5  # 0 to 5
    print(random_float_range)
    ```
    Sample output: `3.182093745`

4. **Picking a Random Element from a List**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    Sample output: `Hola`

5. **Shuffling a List**
    Perfect for card games or any application needing to randomize order.
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    Sample output: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Deep Dive

The `random` module in Python uses a pseudorandom number generator (PRNG), specifically the Mersenne Twister algorithm, which is good for general-purpose applications but not suitable for cryptographic purposes due to its predictability if enough outputs are observed. The `secrets` module, introduced in Python 3.6, offers a better alternative for generating cryptographically strong random numbers, especially useful in security-sensitive applications. For example, generating a secure, random token for a password reset link:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historically, generating random numbers that are truly random has been a challenge in computing, with early methods relying on physical phenomena or manually entered seeds. The development and adoption of algorithms like Mersenne Twister (used by default in Python's `random` module until at least my last knowledge update in 2023) marked significant progress. However, the ongoing search for more secure and efficient algorithms has led to the inclusion of the `secrets` module for cryptography-related tasks. This evolution reflects the growing importance of security in software development and the need for more robust randomness in applications ranging from encryption to secure token generation.
