---
aliases:
- /en/c/generating-random-numbers/
date: 2024-02-03 17:50:18.648341-07:00
description: "Generating random numbers in C involves creating values that are unpredictable\
  \ and follow a specific distribution, such as uniform or normal. This\u2026"
lastmod: 2024-02-18 23:09:11.518495
model: gpt-4-0125-preview
summary: "Generating random numbers in C involves creating values that are unpredictable\
  \ and follow a specific distribution, such as uniform or normal. This\u2026"
title: Generating random numbers
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in C involves creating values that are unpredictable and follow a specific distribution, such as uniform or normal. This capability is crucial for applications ranging from simulations and games to cryptographic operations, where unpredictability or the simulation of real-world randomness is essential.

## How to:

In C, random numbers can be generated using the `rand()` function, which is part of the C standard library `<stdlib.h>.` By default, `rand()` produces pseudo-random numbers in the range from 0 to `RAND_MAX` (a constant defined in `<stdlib.h>`). For more control over the range, programmers can manipulate the output of `rand()`.

Here's a simple example of generating a random number between 0 and 99:

```c
#include <stdio.h>
#include <stdlib.h> // For rand() and srand()
#include <time.h>   // For time()

int main() {
    // Seed the random number generator
    srand((unsigned) time(NULL));

    // Generate a random number between 0 and 99
    int randomNumber = rand() % 100;

    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

Sample output could vary each time you run this program:

```
Random Number: 42
```
To generate random numbers within a different range, you can adjust the modulus operator (`%`) accordingly. For instance, `rand() % 10` generates numbers from 0 to 9.

It is important to note that seeding the pseudo-random number generator (`srand()` call) with the current time (`time(NULL)`) ensures different sequences of random numbers across program executions. Without seeding (`srand()`), `rand()` would produce the same sequence of numbers every time the program is run.

## Deep Dive

The `rand()` function and its seeding counterpart `srand()` have been part of the C standard library for decades. They are based on algorithms that generate sequences of numbers that only appear to be random—hence the term "pseudo-random." The underlying algorithm in `rand()` is typically a linear congruential generator (LCG).

While `rand()` and `srand()` are sufficient for many applications, they have known limitations, especially concerning the quality of randomness and potential predictability. For applications requiring high-quality randomness, such as cryptographic operations, alternatives like `/dev/random` or `/dev/urandom` (on Unix-like systems), or APIs provided by cryptographic libraries, should be considered.

With the introduction of C11, the ISO C standard included a new header, `<stdatomic.h>`, offering a more refined control for concurrent operations, but not directly concerning randomness. For true randomness in C, developers often turn to platform-specific or external libraries that offer better algorithms or make use of hardware entropy sources.

Remember, while `rand()` serves as a simple and accessible means to generate pseudo-random numbers, its uses in modern applications are limited by the quality and predictability of its output. When more robust solutions are required, especially for security-conscious applications, exploring beyond the standard library is highly recommended.
