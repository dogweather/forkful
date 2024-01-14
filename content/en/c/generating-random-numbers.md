---
title:    "C recipe: Generating random numbers"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers play a crucial role in various computer programs and simulations. They are used to generate unique and unpredictable outcomes, making them essential in games, cryptography, and scientific research.

## How To

Generating random numbers in C programming is made easy with the use of the `rand()` function. This function returns a random number between 0 and `RAND_MAX` (a constant defined in the `stdlib.h` library). Here's a simple code example:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  int random_num = rand();
  printf("Random number: %d", random_num);
  return 0;
}
```

The output would look something like this:

```
Random number: 1974616966
```

By default, `rand()` uses the same seed value every time the program is run. This means that the generated random numbers will be the same in every execution. To avoid this, we can use the `srand()` function to set a different seed value each time the program runs. Here's a modified version of the previous code:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h> // for using the time function

int main() {
  srand(time(NULL)); // sets a different seed value each time
  int random_num = rand();
  printf("Random number: %d", random_num);
  return 0;
}
```

Now, the output will change every time the program is executed.

## Deep Dive

The generation of random numbers relies on an algorithm that produces a sequence of numbers that appear to be random. In reality, these numbers are calculated using a mathematical formula based on a starting value called the seed. This means that the same seed will always generate the same sequence of random numbers.

In C programming, the `rand()` function uses a linear congruential generator algorithm. This algorithm takes the previous generated number and applies a set of mathematical operations to it to produce a new number.

However, the `rand()` function has a few limitations. First, the generated sequence of numbers is not entirely random. The numbers follow a predictable pattern and can eventually repeat. Second, the algorithm used is not cryptographically secure, meaning it can be easily predicted or calculated.

To overcome these limitations, more advanced algorithms and methods can be used, such as the Mersenne Twister or the Blum Blum Shub algorithm. These algorithms offer better randomness and security but are more complex to implement.

## See Also

- [`rand()` function documentation](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Pseudorandom number generator Wikipedia page](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Cryptography and PRNGs](https://www.wikiwand.com/en/Cryptography/Symmetric_key#Keeping_the_key_secret_using_pseudorandomness)