---
title:                "Generating random numbers"
html_title:           "C recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a useful function in programming that can be used for various purposes. From creating randomized simulations to selecting random items in a game, having an efficient way to generate random numbers is essential for any programmer.

## How To

One of the simplest ways to generate a random number in C is by using the `rand()` function from the `stdlib.h` library. Here's an example of how it can be implemented in a program:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Setting seed for the random number generator
    srand(time(0));
    
    // Generating a random number between 1 and 10
    int random = (rand() % 10) + 1;
    
    // Printing the result
    printf("Random number: %d", random);
    
    return 0;
}
```

Sample output:

```
Random number: 8
```

The `srand()` function is used to set the seed for the random number generator, which is necessary in order to generate truly random numbers. In the example above, we used the current time as the seed, but any value can be used. Just make sure to only call `srand()` once in your program, preferably at the beginning.

The `rand()` function returns a random integer between 0 and `RAND_MAX`, which is a constant defined in the `stdlib.h` library. In order to generate a random number within a specific range, we use the modulus operator (`%`) to get the remainder of the division of the random number by the desired range. In the example above, we added 1 to the result in order to get a number between 1 and 10 instead of 0 and 9.

## Deep Dive

While the basic method shown above is sufficient for most cases, it's important to note that the `rand()` function doesn't always generate truly random numbers. The numbers generated are pseudo-random, meaning they follow a certain pattern and are not truly random. This can cause issues in some scenarios, such as when security is a concern.

For situations where true randomness is crucial, there are other libraries like `random` and `arc4random` that provide more robust methods for generating random numbers in C. These libraries use more advanced algorithms to ensure truly random numbers are generated. However, they are not as widely available as `rand()` and may require additional setup.

## See Also

- [C Library - <stdlib.h>](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [Pseudo-random number generation - Wikipedia](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Random numbers in C programming - GeeksforGeeks](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)