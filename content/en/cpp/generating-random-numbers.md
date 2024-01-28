---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:06.519268-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in C++ involves creating data that cannot be predicted logically. Programmers use randomness for a variety of tasks, such as simulations, games, security algorithms, and testing scenarios.

## How to:

C++ offers several ways to generate random numbers, primarily through the `<random>` header introduced in C++11, which provides a flexible set of generators and distributions. Here's how to use it:

```C++
#include <iostream>
#include <random>

int main() {
    // Initialize a random engine
    std::random_device rd;
    std::mt19937 gen(rd()); // Mersenne Twister 19937 generator, seeded with rd()

    // Create a distribution range
    std::uniform_int_distribution<> dis(1, 6); // Uniform distribution between 1 and 6

    // Generate and print five random numbers
    for(int n=0; n<5; ++n)
        std::cout << dis(gen) << ' ';
    std::cout << '\n';
  
    return 0;
}
```

Sample output (will vary each run due to randomness):

```
4 1 6 3 5
```

This example demonstrates generating five random integers simulating dice rolls. You can easily adjust the distribution and generator types for different kinds of randomness.

## Deep Dive

Prior to C++11, programmers relied on `rand()` and `srand()` from `<cstdlib>` for random number generation. However, this approach had limitations in terms of predictability, distribution, and thread safety.

The introduction of the `<random>` header addressed these issues by providing a suite of generators (e.g., `std::mt19937` for Mersenne Twister) and distributions (e.g., `std::uniform_int_distribution<>` for uniform distribution). This not only improved the quality and flexibility of random number generation but also its applicability in more sophisticated applications such as cryptography and simulations.

While `<random>` provides substantial benefits over `rand()`, it's worth noting that for cryptographic purposes, specialized libraries like OpenSSL should be considered due to the need for higher security standards.

C++ continues to evolve, and with each version, more efficient and secure ways of handling tasks like random number generation may be introduced. Nonetheless, the current approach using `<random>` remains a robust and versatile solution fitting a wide range of applications.

## See also

### Official Documentation and References
- [C++ `<random>` Header](https://en.cppreference.com/w/cpp/header/random)

### Tutorials and Guides
- **C++ Reference**: [Random number generation](https://www.cplusplus.com/reference/random/)
- **GeeksforGeeks**: [Random Number Generation in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)

### Video Tutorials
- **The Cherno**: [Random Numbers in C++](https://www.youtube.com/watch?v=6TNS--WetLI)
- **CodeBeauty**: [Random Number Generator in C++ Tutorial](https://www.youtube.com/watch?v=7RlVkYGlShQ)
