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

## What & Why?

Generating random numbers in programming refers to the creation of random values or sequences of values. Programmers use this feature to add uncertainty or unpredictability to their code, which can be helpful in various applications, such as games, simulations, and security systems.

## How to:

To generate a random number in Go, we can use the `math/rand` package and its `Intn` function. This function takes an integer as an argument and returns a random number between 0 (inclusive) and the provided integer (exclusive).

```
Go rand.Intn(10)
```

The above code will generate a random number between 0 and 9. We can also use the `math/big` package to generate random numbers with a higher range or precision.

```
Go rand.Int(bn.NewInt(100))
```

In this example, we use the `Int` function to generate a random integer with a range of 0 to 99.

## Deep Dive

Random number generation has been an essential feature of computer programming since its early days. Initially, computers used pseudo-random number generators, which produced a sequence of values based on a fixed initial value called a seed. However, these generators were not truly random as they followed a predictable pattern. Today, most programming languages, including Go, use a cryptographically secure algorithm to generate truly random values.

There are also other methods for generating random numbers, such as using physical processes or mathematical formulas. However, these methods may not be suitable for all applications as they can be slow or introduce bias. Programmers must also be cautious when using random numbers to ensure they are not compromising the security or accuracy of their code.

## See Also

To learn more about generating random numbers in Go, you can check out the official documentation for the `math/rand` and `math/big` packages. You can also explore other sources, such as tutorials or forums, to see how other programmers are using random number generation in their code.