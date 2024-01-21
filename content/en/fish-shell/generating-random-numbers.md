---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:48.973634-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is creating numbers that cannot be reasonably predicted. Programmers use them for tasks like security protocols, games, simulations, or anything that needs unpredictability.

## How to:
In Fish, you can generate random numbers easily:

```Fish Shell
# Generate a random number between 1 and 10
set -l rand_num (random 1 10)
echo $rand_num
```

Sample output could be: `7`

To get a series of random numbers:

```Fish Shell
# Generate 5 random numbers between 1 and 100
for i in (seq 5)
    echo (random 1 100)
end
```

Sample output might be:

```
84
22
67
99
30
```

## Deep Dive
Fish Shell's random number generator has a simple interface but an intricate underlying mechanism. Before Fish Shell 3.0, random functionality was notable for lacking compared to other shells, prompting scripts to leverage external utilities like `awk`.

Today's `random` is a built-in and relies on pseudorandom number generators (PRNGs), which are algorithms that produce sequences of numbers approximating randomness. Despite their deterministic nature, for most purposes, the numbers are "random enough".

Alternatives to `random` include calling external commands like `shuf`, `awk`, or using the `$RANDOM` environment variable in bash for scripts that need to remain cross-shell.

Regarding cryptographic security, you shouldn't use Fish's `random` for that. Instead, use a language or tool designed with cryptography in mind, like OpenSSL. Fish's `random` is more about convenience and quick, simple use cases that don't require cryptographically secure randomness.

## See Also
- [Fish Shell Documentation on `random`](https://fishshell.com/docs/current/cmds/random.html)
- [OpenSSL Random Numbers Generation](https://www.openssl.org/)
- [Wikipedia article on Pseudorandom Number Generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)