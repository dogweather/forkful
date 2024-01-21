---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:24.484887-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers in programming are like digital dice rolls. They're crucial for game development, simulations, security systems, and anywhere we need unpredictability. 

## How to:

Simple random number example:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Seed the random number generator
    srand((unsigned int)time(NULL));
    
    // Generate a random number between 0 and 49
    int randomNumber = rand() % 50;
    printf("Random Number: %d\n", randomNumber);
    
    return 0;
}
```

Sample output might look like:

```
Random Number: 23
```

If you want a number in a different range, just tweak the modulus operator accordingly. Say, `rand() % 100` for 0-99.

## Deep Dive

Before the C standardization in 1989, random number generation was more like the Wild West – different systems had different methods. Now, we've got a standard library (stdlib.h) and a starting point (srand) that uses the system clock (time.h) to avoid predictability.

Alternatives to `rand()` include functions like `random()`, which may offer better randomness on some systems. There are also cryptographic libraries for security-sensitive applications, which require higher quality randomness than `rand()` can provide.

Implementation detail to remember: `rand()` isn't truly random; it's pseudo-random, which means it's determined by an initial value (the seed). Hence why seeding with the current time is common practice – it's different every second.

## See Also

For more adventures in randomness:

- C Standards documentation for `<stdlib.h>`: https://en.cppreference.com/w/c/numeric/random
- A deeper look into pseudo-random number generators (PRNGs): https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- For cryptographic applications, see libsodium's documentation: https://doc.libsodium.org/generating_random_data