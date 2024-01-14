---
title:                "C++ recipe: Generating random numbers"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is an essential skill for any programmer. It allows for the creation of realistic and unpredictable data, which is crucial for testing and simulations. It is also commonly used in games, cryptography, and statistical analysis. In this blog post, we will go over the basics of random number generation in C++.

## How To

The first step in generating random numbers in C++ is to include the `<random>` library. This library provides several classes and functions for generating random numbers. The most basic way is to use the `std::rand()` function, which returns a pseudo-random number between 0 and `RAND_MAX`.

```C++
#include <iostream>
#include <cstdlib>

int main() {

    // Generate and print a random number
    int randomNum = std::rand();
    std::cout << "Random number: " << randomNum << std::endl;

    return 0;
}
```

The above code will produce a different random number each time it is executed. However, the `rand()` function is not recommended for serious applications as it has a limited range and potential for biases. Instead, the `<random>` library provides better options.

The `std::random_device` class uses a hardware source of randomness, such as a physical random number generator, for more unpredictable results. It is recommended to use this as a seed for other random number generators.

```C++
#include <iostream>
#include <random>

int main() {

    // Use random_device as a seed for mt19937
    std::random_device rd;
    std::mt19937 gen(rd());

    // Generate and print a random number
    int randomNum = gen();
    std::cout << "Random number: " << randomNum << std::endl;

    return 0;
}
```

This code uses a high-quality random number generator, `std::mt19937`, which produces a wider range of numbers compared to the `rand()` function. Other distributions, like `std::uniform_int_distribution` and `std::uniform_real_distribution`, can be used to specify a range for the generated numbers.

## Deep Dive

The `<random>` library uses pseudo-random number generators, which produce a sequence of numbers based on a starting value called the seed. The same seed will always produce the same sequence of numbers, hence the term pseudo-random. However, these numbers appear indistinguishable from truly random numbers for most applications.

It is crucial to note that the random number generators in the `<random>` library are deterministic. This means that given the same seed, the same sequence of numbers will be generated. For truly random numbers, an external source of entropy, like user input or system time, can be used as a seed.

## See Also

- [C++ Random Numbers](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [C++ Reference: `<random>` library](https://en.cppreference.com/w/cpp/numeric/random)