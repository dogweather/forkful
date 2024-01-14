---
title:                "C recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate a random number in your C program? Whether it's for a game or to select a random item from a list, generating random numbers is a useful skill to have in your programming arsenal.

## How To

To generate random numbers in C, we will be using the `rand()` function from the `stdlib.h` library. This function returns a pseudo-random number between 0 and `RAND_MAX` (a constant defined in `stdlib.h`).

To use this function, we first need to seed it with a random value by using the `srand()` function. This function takes in an integer value, which will be used as the seed for the `rand()` function. This seed value can be anything, but it's common to use the current time as it changes every second.

Let's take a look at a simple example:

```C
#include <stdio,h>
#include <stdlib.h>

int main() {

    // Seed the rand() function with the current time
    srand(time(NULL));

    // Generate and print a random number between 1 and 10
    int random_num = rand() % 10 + 1;
    printf("Random number between 1 and 10: %d", random_num);

    return 0;
}
```

Output:

```
Random number between 1 and 10: 7
```

As you can see, we have used the `rand()` function to generate a random number between 1 and 10. By using the modulus operator and adding 1, we ensure that the random number will always be between 1 and 10.

## Deep Dive

While the `rand()` function is great for most applications, it is not truly random. It is a pseudo-random number generator, meaning it follows a predetermined sequence of numbers based on the seed value. This sequence can be replicated, making it predictable.

To get a more random number, we can use the `rand()` function in combination with other algorithms or add our own logic to it. For example, we can use the current time in milliseconds as the seed value, or we can implement a more complex mathematical formula to generate a more unpredictable sequence of numbers.

It's also important to note that `rand()` is not thread-safe, meaning it can lead to unexpected results when used in multi-threaded programs. For thread-safe random number generation, we can use the `rand_r()` function, which takes in a pointer to a seed value as an additional parameter.

## See Also

- [C Library - <stdlib.h>](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [Pseudo-random number generation in C](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [Thread-safe random numbers in C](https://www.gnu.org/software/libc/manual/html_node/Primitive-Roots.html#Primitive-Roots)