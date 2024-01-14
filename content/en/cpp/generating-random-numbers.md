---
title:    "C++ recipe: Generating random numbers"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Random numbers are a fundamental aspect of computer programming. They add an element of unpredictability, making programs more dynamic and interesting. Additionally, they have practical applications such as generating test data or simulating real-world scenarios. In this blog post, we will explore how to generate random numbers in C++.

## How To

To generate random numbers in C++, we will use the standard library function `rand()`. This function returns a random number between 0 and `RAND_MAX`, which is typically a very large number. However, we can manipulate the output to fit our specific needs.

```C++
#include <iostream>
#include <cstdlib> // included for using `rand()`
using namespace std;

int main() {
    // generate a random number between 0 and 9
    int randomNumber = rand() % 10;
    cout << "Random number: " << randomNumber << endl;
    
    // generate a random number between 1 and 100
    int min = 1;
    int max = 100;
    int range = max - min + 1;
    randomNumber = (rand() % range) + min;
    cout << "Random number: " << randomNumber << endl;
    
    return 0;
}
```

In the above code, we first include the necessary libraries, `iostream` for input and output and `cstdlib` for the `rand()` function. We then generate a random number between 0 and 9, and print it to the console. Next, we show how to generate a random number within a specific range, in this case, between 1 and 100.

## Deep Dive

The `rand()` function uses a pseudorandom number generator algorithm to produce seemingly random numbers. However, this algorithm is deterministic, which means that the same seed value will produce the same sequence of numbers. To mitigate this, we can use the `srand()` function to set a different seed value each time the program runs.

Another important aspect to consider when generating random numbers is their distribution. The `rand()` function returns equally distributed numbers, but sometimes we need a different distribution, such as a normal distribution. This can be achieved using more advanced techniques, such as the Box–Muller transform, which is outside the scope of this blog post.

## See Also

- [C++ `rand()` documentation](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [Generate random numbers in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Box–Muller transform explanation](https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform)