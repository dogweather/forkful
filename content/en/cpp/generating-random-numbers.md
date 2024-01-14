---
title:                "C++ recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever played a game or used an app where the outcome seemed completely random? Chances are, the developers used computer-generated random numbers to add an element of unpredictability to their program. In this blog post, we'll explore the world of generating random numbers with C++.

## How To

Creating random numbers in C++ is relatively simple. First, we need to include the `<cstdlib>` library, which contains the necessary functions for generating random numbers. Then, we can use the `rand()` function to generate a random number between 0 and `RAND_MAX`, which is a constant defined in the library. However, this will result in the same sequence of numbers each time the program is run.

To create a more truly random sequence, we can use the `srand()` function to seed the random number generator with a changing value, such as the system clock time. This will produce a different sequence of numbers each time the program is run.

Let's see an example of generating and printing 10 random numbers between 1 and 100:

```
#include <iostream>
#include <cstdlib>

int main() {
  // seed the random number generator
  srand(time(0));

  // generate and print 10 random numbers
  for (int i = 0; i < 10; i++) {
    int randomNumber = rand() % 100 + 1; // generate random number between 0 and 100
    std::cout << randomNumber << std::endl;
  }

  return 0;
}
```

Output:

```
72
13
56
92
31
24
17
41
88
9
```

## Deep Dive

Generating truly random numbers is a complex task that relies on mathematical algorithms called random number generators (RNGs). These algorithms use a starting value, called a seed, to generate a sequence of numbers that appear to be random.

C++ has two types of RNGs: the linear congruential generator (LCG) and the Mersenne Twister. The `rand()` function we used earlier is based on LCG, which is a simple and fast algorithm but has some limitations in terms of randomness. The Mersenne Twister, on the other hand, is a more sophisticated algorithm that produces more evenly distributed and unpredictable numbers.

It's essential to understand that computer-generated random numbers are actually pseudo-random, meaning they are not truly random but appear to be. This is because computers operate on a set of instructions and cannot truly generate random numbers without an external input.

## See Also

- [Random number generation in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C++ random number generators](http://www.cplusplus.com/reference/random/)
- [The science of random](https://www.random.org/randomness/)