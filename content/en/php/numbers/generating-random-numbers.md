---
title:                "Generating random numbers"
aliases:
- /en/php/generating-random-numbers.md
date:                  2024-01-27T20:26:30.387709-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in PHP is about producing unpredictable values within a specified range, which is essential for tasks like creating unique user IDs, generating passwords, or for use in simulations and games. Programmers rely on randomness to add unpredictability and variability into their applications, making processes like testing or user experiences more robust and engaging.

## How to:

PHP offers several functions for generating random numbers, but the most commonly used are `rand()`, `mt_rand()`, and for cryptographic purposes, `random_int()`.

To generate a simple random number between 0 and getrandmax() (the largest possible value returned by `rand()`), you can use:

```PHP
echo rand();
```

For a more specific range, such as between 1 and 100:

```PHP
echo rand(1, 100);
```

However, `mt_rand()` is a better choice for speed and randomness:

```PHP
echo mt_rand(1, 100);
```

Output for both could be anything between 1 and 100, depending on the randomization, e.g., `42`.

For cryptographic or security contexts, where unpredictability is crucial, `random_int()` is the preferred choice as it generates cryptographically secure pseudo-random integers:

```PHP
echo random_int(1, 100);
```

Again, the output is a random number between 1 and 100, like `84`, but with a stronger guarantee of randomness.

## Deep Dive

The `rand()` function has been present in PHP since its early versions, serving as the initial approach for generating random numbers. However, it's not the best choice for applications requiring a high degree of randomness due to its relatively predictable algorithm.

`mt_rand()`, introduced in PHP 4, is based on the Mersenne Twister algorithm - far superior in terms of speed and the randomness it can generate compared to `rand()`. It quickly became the preferred option for most non-cryptographic needs.

For security-sensitive applications, `random_int()` was introduced in PHP 7 to generate cryptographically secure pseudo-random integers using random bytes from the system's random number generator. It is significantly more secure than `rand()` or `mt_rand()`, making it the best choice for generating tokens, keys, or other elements where predictability could lead to security vulnerabilities.

Despite these improvements, it's crucial to choose the right function based on the application's context. For general use, `mt_rand()` suffices, but for anything that could be targeted or exploited, `random_int()` is the way to go, providing both randomness and security.
