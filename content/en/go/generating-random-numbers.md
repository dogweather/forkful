---
title:    "Go recipe: Generating random numbers"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Generating random numbers is a common task in many programming languages, including Go. Random numbers can be used for a variety of purposes, such as creating encryption keys, simulating dice rolls, or shuffling data. Learning how to generate random numbers in Go can be useful for a wide range of applications.

## How To

In Go, the `math/rand` package provides functions for generating pseudo-random numbers. These numbers are not truly random, but they are generated using a deterministic algorithm that gives the appearance of randomness. This package also allows for better control over the seed used for generating the numbers, which can be useful for testing purposes.

To start generating random numbers in Go, we first need to import the `math/rand` package. Then, we can use the `Intn()` function to generate a random integer within a given range. For example, to generate a random number between 1 and 100, we can use the following code:

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // set seed for deterministic results (optional)
    rand.Seed(42)
    
    // generate a random number between 1 and 100
    num := rand.Intn(100) + 1
    
    fmt.Printf("Random number: %d\n", num) // output: Random number: 83
}
```

In the above example, we use `Intn()` to generate a random integer within the range of 0 to 99, and then we add 1 to the result to get a range of 1 to 100. We can also use other variations of `Intn()` to generate different types of random numbers, such as signed integers or numbers within a specific range.

Besides integers, we can also generate random float numbers using the `Float64()` function. Here's an example of generating a random float number between 0 and 1:

```Go
// generate a random float number between 0 and 1
rand.Float64()
```

## Deep Dive

While the `math/rand` package is great for most general purpose applications, it's important to understand that the numbers generated are not truly random. They are generated using a mathematical algorithm, which means that the same seed will always result in the same sequence of numbers. This can be useful for testing or debugging purposes, but it's not ideal for security-related tasks such as generating encryption keys or generating unique identifiers.

For more secure and truly random numbers, Go provides the `crypto/rand` package. This package uses system-specific entropy to generate random numbers, making them more unpredictable and secure. However, it's important to note that using this package can be slower and may not be necessary for all applications.

## See Also

- [Go documentation on `math/rand` package](https://golang.org/pkg/math/rand/)
- [Go documentation on `crypto/rand` package](https://golang.org/pkg/crypto/rand/)
- [Random number generation in other programming languages](https://en.wikipedia.org/wiki/Random_number_generation)