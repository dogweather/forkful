---
date: 2024-01-27 20:26:09.179228-07:00
description: "How to: To generate random numbers in C++, you'd typically make use\
  \ of the `<random>` header, which was introduced in C++11, offering a wide range\
  \ of\u2026"
lastmod: '2024-03-13T22:45:00.353910-06:00'
model: gpt-4-0125-preview
summary: To generate random numbers in C++, you'd typically make use of the `<random>`
  header, which was introduced in C++11, offering a wide range of facilities for generating
  random numbers from various distributions.
title: Generating random numbers
weight: 12
---

## How to:
To generate random numbers in C++, you'd typically make use of the `<random>` header, which was introduced in C++11, offering a wide range of facilities for generating random numbers from various distributions.

```C++
#include <iostream>
#include <random>

int main() {
    // Initialize a random engine
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Define the range [0, 99] inclusive
    std::uniform_int_distribution<> distrib(0, 99); 

    // Generate and print 5 random numbers within the defined range
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

This code sample initializes a Mersenne Twister random number generator with a seed from `std::random_device`. It then defines a uniform integer distribution in the range [0, 99] and finally prints out 5 random numbers from this distribution.

Sample output might look like this, but keep in mind every execution will likely produce different results:

```
45 67 32 23 88
```

## Deep Dive:
Historically, random number generation in C++ relied heavily on the `rand()` function and the `srand()` function for seeding, found in the `<cstdlib>` header. However, this approach often faced criticism for its lack of uniformity and predictability in the distribution of generated numbers.

The introduction of the `<random>` header in C++11 marked a significant improvement, offering a sophisticated system for producing random numbers. The facilities provided include a variety of engines (like `std::mt19937` for Mersenne Twister) and distributions (like `std::uniform_int_distribution` for uniform distribution of integers) that can be combined to suit the programmer's specific needs, leading to more predictable behavior, better performance, and greater flexibility.

While the `<random>` library is much better than the older `rand()` approach, it's worth noting that generating truly random numbers—especially for cryptographic purposes—still relies on additional considerations. For cryptographic applications, libraries designed specifically for security, which often utilize hardware entropy sources, should be used instead.
