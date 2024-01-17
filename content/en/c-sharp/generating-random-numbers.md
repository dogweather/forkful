---
title:                "Generating random numbers"
html_title:           "C# recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is the process of producing a sequence of numbers that have no discernible pattern. This can be useful in a variety of programming applications, such as simulations, games, and cryptography. Programmers use random numbers to add unpredictability and variability to their programs.

## How to:
To generate random numbers in C#, you can use the ```Random``` class from the ```System``` namespace. Here's how you can generate a random integer between 1 and 10:

```C#
Random random = new Random();
int randomNumber = random.Next(1, 11);
```

You can also use the ```NextDouble()``` method to generate a random double between 0.0 and 1.0:

```C#
double randomDouble = random.NextDouble();
```

If you want to generate a random number within a specific range, you can use some simple math to adjust the output of the ```Next()``` or ```NextDouble()``` methods. For example, to generate a random number between 5 and 10, you can do the following:

```C#
int randomNumber = random.Next(5, 11);
// OR
double randomDouble = (random.NextDouble() * 5) + 5;
```

## Deep Dive:
Generating random numbers has been a challenge for programmers for a long time. In the early days of computing, computers were deterministic, meaning they follow a set of logical instructions to produce predictable outputs. This made it difficult to generate truly random numbers. 

In the mid-20th century, mathematicians and computer scientists came up with various algorithms to generate pseudo-random numbers, which are numbers that appear to be random but are actually produced by a deterministic process. These algorithms are used in modern programming languages, including C#.

While the ```Random``` class is the most commonly used method for generating random numbers in C#, there are other alternatives. For example, developers can also use cryptographic random number generators for more secure applications. Additionally, there are third-party libraries available that offer more advanced random number generation capabilities.

The implementation of random number generation in C# is based on the linear congruential generator algorithm, which uses modular arithmetic to generate a sequence of numbers that appear random. This algorithm has its limitations and can eventually lead to predictable patterns, so it's important to use it carefully.

## See Also:
- [Random numbers in C#](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [Pseudo-random numbers](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Cryptographic random number generation](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
- [Third-party random number generators for C#](https://www.nuget.org/packages?q=Random+Number+Generator&packageType=Libraries)