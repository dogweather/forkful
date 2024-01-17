---
title:                "Generating random numbers"
html_title:           "C++ recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is a process where a computer program generates a seemingly random sequence of numbers. Programmers do this for a variety of reasons such as creating randomized passwords, simulating realistic scenarios in games, or testing the performance of algorithms.

## How to:
To generate random numbers in C++, we can use the ```rand()``` function from the ```cstdlib``` library. This function takes no arguments and returns a pseudo-random integer between 0 and ```RAND_MAX```. We can also use the ```srand()``` function to set a seed value for the random number generator.

Example code:
```
#include <cstdlib> // including the necessary library
#include <iostream>

int main() {
  // setting the seed value to 42
  srand(42);
  
  // generating a random number
  int num = rand();
  
  // printing the number
  std::cout << "A random number: " << num << std::endl;
  
  return 0;
}
```

Sample output:
```
A random number: 465
```

## Deep Dive:
The ```rand()``` function uses a pseudo-random number generator (PRNG) algorithm to generate the sequence of numbers. This means that the numbers are not truly random, but they appear to be random for most purposes. The algorithm used by ```rand()``` is not specified by the C++ standard, so it may vary depending on the compiler and implementation.

There are also other ways to generate random numbers in C++, such as using the ```random``` library or using third-party random number generator libraries. These may provide different and potentially more robust methods for generating random numbers.

## See Also:
- [C++ Reference for rand() function](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ Reference for srand() function](https://www.cplusplus.com/reference/cstdlib/srand/)
- [C++ Reference for random library](https://www.cplusplus.com/reference/random/)
- [Third-party random number generator libraries for C++](https://www.slant.co/topics/1792/~c-libraries-for-random-number-generation)