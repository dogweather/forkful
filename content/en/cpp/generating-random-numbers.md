---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generating Random Numbers in C++

## What & Why?
Generating random numbers means producing sequences that have no discernible patterns. Programmers use this for creating test datasets, gaming, simulations, cryptography, and even in programs that need unique IDs.

## How to:
```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd; //Creating random_device object
    std::mt19937 gen(rd()); //Seed mt19937 generator
    std::uniform_int_distribution<> distr(1,6); //Defining the range

    for (int i = 0; i < 10; ++i) {
        std::cout << distr(gen) << " "; //Generating and printing random numbers in range 1-6
    }
    
    return 0;
}
```

Example of possible output:

```C++
3 6 1 1 5 2 6 3 4 4 
```

## Deep Dive
Historically, programmers would use the rand() function in C++, but it's not truly random, repeating its sequence every 32767 numbers. In modern C++, `<random>` library provides better tools, like `std::mt19937` generator that gives us a pretty good 'random' generator with period 2^19937-1. Large period and ability to produce numbers in the required range directly helps reduce pitfalls of traditional methods.

Another worthy mention is `std::random_device`. It's much more random than `std::mt19937`, but typically slower and could be non-random on some systems, so it's used to seed an `mt19937` generator.

Programmers can choose to generate random integers or floating-point numbers, vary the distribution, or even create custom distributions, based on their use case.

## See Also
Use these resources for more:
- Visit [CPP Reference](https://en.cppreference.com/w/cpp/numeric/random) for more details on `<random>`.
- Use [this guide](https://www.codespeedy.com/generating-random-float-numbers-in-cpp/) to generate random floating-point numbers.
- Check out [this](https://www.cplusplus.com/reference/random/) for a detailed explanation on random number distributions in C++.