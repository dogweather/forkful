---
title:                "Generating random numbers"
html_title:           "Go recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why
Generating random numbers is a common task in programming, especially when working with games, simulations, and statistical analysis. Using Go's built-in random number generation functions can make this process easy and efficient.

## How To
To generate random numbers in Go, we can use the `rand` package. First, we need to import the package:

```Go
import "math/rand"
```

### Generating a Single Random Number
To generate a single random number, we can use the `rand.Intn()` function. This function takes in an integer n as a parameter and returns a random integer between 0 and n-1. Here's an example of how we can use it:

```Go
rand.Seed(time.Now().UnixNano()) //seeds the randomness
randomNum := rand.Intn(10) //generates a random number between 0 and 9
fmt.Println(randomNum) //output: 7 (number will vary)
```

### Generating a Random Number within a Range
If we want to generate a random number within a specific range, we can use the `rand.Intn()` function in combination with basic arithmetic. For example, to generate a random number between 5 and 10, we can use the following code:

```Go
rand.Seed(time.Now().UnixNano())
randomNum := (rand.Intn(6) + 5) //adds 5 to the generated random number between 0 and 5
fmt.Println(randomNum) //output: a random number between 5 and 10
```

### Generating a Random Float
To generate a random floating-point number, we can use the `rand.Float64()` function. This function returns a random number between 0.0 and 1.0. Here's an example:

```Go
rand.Seed(time.Now().UnixNano())
randomNum := rand.Float64() //generates a random float between 0.0 and 1.0
fmt.Println(randomNum) //output: 0.545981 (float will vary)
```

## Deep Dive
The `rand` package in Go uses a mathematical algorithm known as a "pseudo-random number generator" to generate random numbers. This means that the numbers produced are not truly random, but they appear to be random enough for most practical purposes. The `Seed()` function is used to initialize the generator with a specific value, called a "seed". By seeding the generator, we can produce a different sequence of numbers each time, as shown in the examples above using the current time as the seed. It is important to note that seeding the generator with the same value will produce the same sequence of numbers, so it is recommended to use a different seed value each time.

## See Also
- Documentation on the `rand` package in Go: https://golang.org/pkg/math/rand/
- Explanation of pseudo-random number generators: https://en.wikipedia.org/wiki/Pseudorandom_number_generator