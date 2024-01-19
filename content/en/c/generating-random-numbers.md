---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Let’s hit the ground running. Generating random numbers involves creating a sequence that can’t be reasonably predicted. Coders do it to seed encryption algorithms, create random IDs, perform random testing, simulate unpredictable behavior, etc., All in all, it's a handy tool to have.

## How to:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0)); // seed with current time
    int random_num = rand(); // generate random number
    printf("%d\n", random_num); // print it out
    return 0;
}
```
Don't overthink it—just tweak the seed to change the random sequence. Pop in `time(0)` as a seed and you got pseudorandom numbers. Run it, and voila, a surprise integer.

## Deep Dive

In the early C era, random number generation wasn't cool along the lines of today. Coders mostly used `rand()`, a simple linear congruential generator. It's historical—and rather crude.

There are more sophisticated ways to generate random (more like pseudorandom) numbers. For instance, the Mersenne Twister algorithm yields a sequence with a ridiculously long period—`2^19937−1`—way more unpredictable.

Under the hood, `rand()` operates on a seed value. If the seed remains the same, `rand()` provides the same sequence. Mix it up—use `srand()`—to change the seed and thus the sequence. Remember our friend `time(0)` from before? It can act as an ever-changing seed.
    
Remark: if you use `rand()` for sensitive stuff, like cryptography, you're in for a bad time. It's not cryptographically secure.

## See Also
To understand more, take a peek at the reroll, so to speak:
- [`rand()` and `srand()`](https://en.cppreference.com/w/c/numeric/random/rand)
- [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Cryptographically Secure Pseudorandom Number Generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)

Remember: the right tool for the right job. Keep coding cool!