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

## Why

Need to add some randomness to your program? Want to simulate real-world scenarios? Generating random numbers can add a touch of unpredictability and help you create more dynamic and realistic applications.

## How To

Generating random numbers in C++ is fairly straightforward. All you need is the `rand()` function from the standard library and some basic understanding of data types.

To get a random integer between 0 and a specific number, say 10, you can use the following code:

```C++
int random_number = rand() % 11; // will generate a number between 0 and 10
```

To generate a random float between 0 and 1, you can divide the output of `rand()` by `RAND_MAX` (a constant defined in the `cstdlib` library):

```C++
float random_float = static_cast<float>(rand()) / RAND_MAX;
```

You can also use the `srand()` function to set a seed for your random numbers. This can be useful if you want to produce the same sequence of random numbers each time you run your program. All you need to do is pass a specific value to `srand()` before calling `rand()`, like this:

```C++
srand(1234); // sets the seed to 1234
int random_number = rand(); // will always generate the same output
```

You can also use the `time()` function to set a seed based on the current time, making your random numbers truly unpredictable:

```C++
srand(time(0)); // sets the seed based on the current time
int random_number = rand(); // will produce different output every time you run the program
```

Here's a sample program to generate 10 random integers between 1 and 100 and print them out:

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

using namespace std;

int main()
{
    // set a seed based on the current time
    srand(time(0));

    // generate 10 random numbers and print them out
    for (int i = 0; i < 10; i++)
    {
        int random_number = rand() % 100 + 1; // generate a number between 1 and 100
        cout << random_number << " ";
    }

    return 0;
}
```

Output:
```
37 89 5 72 12 44 61 33 15 97
```

## Deep Dive

Behind the scenes, the `rand()` function uses a pseudo-random number generator to generate random numbers. This means that the sequence of numbers it produces is actually deterministic and not truly random. It uses a mathematical algorithm to generate the numbers, starting with a specific initial value called a seed.

When you use `srand()` to set a seed, you are essentially setting the starting point for the sequence of numbers. Different values for the seed will result in different sequences of numbers, but the same seed will always produce the same sequence.

It's important to note that the `rand()` function is not suitable for cryptography or security-related purposes, as its pseudo-random nature can make it predictable. For such use cases, it's best to use a dedicated cryptographic library.

## See Also

- [C++ documentation on random numbers](https://en.cppreference.com/w/cpp/numeric/random)
- [Generating pseudo-random numbers in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Advanced random number generation in C++](https://www.martinbroadhurst.com/generating-random-numbers-in-c.html)