---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:48:30.227444-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Generating random numbers in C allows programs to act unpredictably, which is key for simulations, games, security systems, and more. We use randomization to mimic the chaos of life or protect data with entropy.

## How to: (Як це робити:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Seed the random number generator
    srand((unsigned int)time(NULL));
  
    // Generate and print a random number
    int rand_num = rand() % 50; // Random number between 0 and 49
    printf("Random Number: %d\n", rand_num);
  
    return 0;
}
```
Sample Output:
```
Random Number: 23
```
To get a different number each time, seed `srand()` with `time(NULL)`. The `% 50` limits the number to 0-49. Remove it for the full range.

## Deep Dive (Поглиблений Розбір):
Early computers lacked randomness, relying on algorithms for "pseudo-random" numbers. Remember, numbers from `rand()` aren't truly random; they're *predictable* if you know the seed. There are alternatives like `/dev/random` and `/dev/urandom` in Unix-like systems for more entropy, while libraries like OpenSSL improve cryptographically secure random number generation.

C's `rand()` function gives a good-enough solution most of the time but beware of patterns. For robust needs, explore libraries designed for your goal, whether it's gaming, cryptography, or simulations. For non-critical randomness, `rand()` suffices, but initialize your seed thoughtfully!

## See Also (Дивіться Також):
- C Standard Library Documentation on `rand()`: https://en.cppreference.com/w/c/numeric/random/rand
- Wikipedia on Pseudo-Random Number Generators: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- OpenSSL for Cryptographic Random Numbers: https://www.openssl.org/