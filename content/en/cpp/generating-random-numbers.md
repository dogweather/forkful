---
title:    "C++ recipe: Generating random numbers"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Generating random numbers is a fundamental aspect of programming and can be used in a variety of applications. Whether it's for creating randomized elements in a game or generating unique results in data analysis, having the ability to generate random numbers can greatly enhance the functionality of your code.

## How To

To start generating random numbers in C++, we first need to call the ```rand()``` function from the ```<cstdlib>``` library. This function will return a pseudo-random number between 0 and the maximum value of ```RAND_MAX```, which is at least 32767.

Here's a simple example of how to use the ```rand()``` function to generate 5 random numbers:

```C++
#include <iostream>
#include <cstdlib>

int main() {
    // generate 5 random numbers
    for (int i = 0; i < 5; ++i) {
        int random_number = rand();
        std::cout << random_number << std::endl;
    }
    return 0;
}

// Output:
// 27928
// 42462
// 21102
// 33711
// 29826
```

As you can see, each time we run the code, the generated numbers are different. This is because the ```rand()``` function uses the system clock as a seed, making the numbers appear to be truly random.

However, if we want to control the range of the random numbers, we can use some basic arithmetic. Let's say we want to generate random numbers between 1 and 10. We can achieve this by using the modulus operator with 10 and adding 1 to the result, as shown in the following example:

```C++
int random_number = (rand() % 10) + 1;
```

We can also use the ```srand()``` function to set a specific seed for our random numbers instead of using the system clock. This is useful if we want to generate the same set of random numbers every time we run the code. Here's an example of how to use the ```srand()``` function:

```C++
srand(1234); // set seed to 1234
int random_number = rand();
```

## Deep Dive

Behind the scenes, the ```rand()``` function uses a mathematical algorithm to generate the random numbers. This algorithm is called a pseudo-random number generator (PRNG) because the numbers it produces are not truly random, but they appear to be random. Different compilers may use different PRNG algorithms, but they all follow the same basic principles.

It's worth noting that true randomness is difficult to achieve in a computer program, as computers are based on deterministic processes. That's why pseudo-random numbers are used instead, as they provide a good enough level of randomness for most applications.

Another important aspect to note when generating random numbers is the concept of seeding. As mentioned before, the seed is used to initialize the PRNG algorithm and can greatly affect the sequence of generated numbers. Using the same seed will always result in the same sequence of numbers, which can be useful for debugging purposes or when generating random elements for a game.

## See Also

- [C++ documentation for rand() function](https://en.cppreference.com/w/cpp/numeric/random/rand)
- [C++ documentation for srand() function](https://en.cppreference.com/w/cpp/numeric/random/srand)
- [Pseudo-random number generator (Wikipedia)](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)