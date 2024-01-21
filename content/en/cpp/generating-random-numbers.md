---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:33.155054-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers are the bread and butter in simulations, games, testing, and security. They help make things unpredictable, simulating reality, or shaking up our code in ways that reveal its weaknesses or strengths.

## How to:

C++ has come a long way in random number generation. Gone are the days of `rand()` being your only quick fix. Now, we've got the `<random>` header which gives us a whole smorgasbord of randomness goodness.

```C++
#include <iostream>
#include <random>

int main() {
    // Initialize our mersenne twister with a random seed based on the system clock
    std::mt19937_64 rng(std::random_device{}());

    // Uniformly-distributed integer range from 1 to 6 (like a dice)
    std::uniform_int_distribution<std::mt19937_64::result_type> dist6(1, 6);

    // Generate and print a random number
    std::cout << "Random dice roll: " << dist6(rng) << std::endl;

    return 0;
}
```

Run the code and you'll get something like "Random dice roll: 4". Run it again, and who knows? That's the point.

## Deep Dive

Before `<random>`, `rand()` was all the rage. However, it was infamous for poor distribution and platform-specific quirks. With C++11, we got `<random>`, offering better algorithms and distributions.

Modern C++ lets us pick our pseudo-random number generator (PRNG). For example, `std::mt19937` is a Mersenne Twister - great for simulations and games, not so much for cryptography.

Other options? Plenty. `std::uniform_int_distribution` is for integers, while similar tools exist for different data types and distributions, like normal or binomial.

The beauty lies in seeding. `std::random_device` is often used to provide a non-deterministic seed, but if you want reproducible results, just hardcode a seed yourself.

Still, remember that if security is key, `<random>` isn't your best friend. That's when you turn to libraries designed for cryptography.

## See Also

- [cppreference.com](https://en.cppreference.com/w/cpp/header/random) – Your go-to for details on the `<random>` library.
- [cryptography.io](https://cryptography.io/en/latest/) – A library for cryptographic tasks if `<random>` doesn’t cut it.
- [isocpp.org](https://isocpp.org/) — For the latest on what's happening in the world of C++.