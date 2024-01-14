---
title:    "Go recipe: Generating random numbers"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a key aspect of many programming languages, including Go. Random numbers are useful for a variety of applications, such as creating randomized games, simulations, and cryptographic key generation. By understanding how to generate random numbers in Go, you can add a useful tool to your programming arsenal.

## How To

Generating random numbers in Go is a straightforward process. The first step is to import the "math/rand" package, which contains functions for generating random numbers. Then, you can use the "Intn(n int)" function to generate an integer between 0 and n-1. For example, to generate a random number between 1 and 100, you would use the following code:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  // Generate a random number between 1 and 100
  num := rand.Intn(100) + 1
  fmt.Println("Random number:", num)
}
```

The output of this code could be something like: "Random number: 67". Keep in mind that the result will be different each time you run the code, as it is a random number.

If you want to generate a random number within a specific range, you can use the "Intn(max-min)+min" formula. For example, to get a random number between 10 and 20, you would use the following code:

```Go
// Generate a random number between 10 and 20
num := rand.Intn(20-10) + 10
```

You can also generate randomized floating-point numbers using the "Float64()" function. This function will generate a number between 0 and 1. To get a random floating-point number between 0 and 10, you would use the following code:

```Go
// Generate a random floating-point number between 0 and 10
randomNum := rand.Float64() * 10
```

## Deep Dive

Random numbers may seem simple on the surface, but there is actually a lot of complexity behind how they are generated. Go uses a mathematical algorithm known as a pseudo-random number generator (PRNG) to generate random numbers. This algorithm takes a starting seed number and generates a sequence of numbers that appear to be random. However, the sequence is actually deterministic, meaning that the same starting seed will result in the same sequence of numbers.

In Go, the default PRNG is based on the linear congruential generator (LCG) method, which uses a linear equation to generate the next number in the sequence. While this method is fast and efficient, it does have some limitations in terms of the randomness of the numbers generated. For more advanced applications, you may need to use a different PRNG or even a true random number generator (TRNG) to ensure a higher level of randomness.

## See Also

- [Go documentation on random numbers](https://golang.org/pkg/math/rand/)
- [Article on randomness in Go](https://www.calhoun.io/creating-random-strings-in-go/)
- [Go Playground with sample code](https://play.golang.org/p/TjS-vVRWQEI)