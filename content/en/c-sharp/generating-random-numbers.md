---
title:                "C# recipe: Generating random numbers"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to generate random numbers for a project or program? Maybe you want to create a guessing game or simulate dice rolls. Generating random numbers is a useful skill to have in any programming language, including C#. 

## How To

Generating random numbers in C# is simple and straightforward. First, we need to include the `Random` class in our code:
```C#
using System;

Random random = new Random();
```
Next, we can use the `Next` method of the `Random` class to generate a random integer within a specified range:
```C#
int randomNumber = random.Next(1,10);
```
In this example, the `Next` method will generate a random integer between 1 and 10. To generate a random float, we can use the `NextDouble` method:
```C#
double randomFloat = random.NextDouble();
```
We can also use the `NextBytes` method to generate an array of random bytes:
```C#
byte[] randomBytes = new byte[10];
random.NextBytes(randomBytes);
```
Now, let's see the results of our code:
```C#
Console.WriteLine($"Random number: {randomNumber}");
Console.WriteLine($"Random float: {randomFloat}");
Console.WriteLine($"Random bytes: {string.Join(", ", randomBytes)}");
```
Output:
```
Random number: 7
Random float: 0.56733
Random bytes: 99, 55, 224, 16, 190, 231, 175, 76, 122, 93 
```

## Deep Dive

The `Random` class uses a mathematical algorithm to generate random numbers based on a given seed value. A seed is a starting point for the algorithm and using the same seed will result in the same sequence of random numbers. By default, the seed value is based on the current time. However, we can set our own seed value using the `Random` class constructor:
```C#
Random random = new Random(1234);
```
It's important to note that the generated random numbers are not truly random, as they are determined by an algorithm. They are considered pseudo-random numbers. 

To generate a random number within a range that includes both of the specified limits, we can use the `Next` method with an exclusive upper limit:
```C#
int randomNumber = random.Next(10,20);
```

## See Also

- [Random class in C# documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Different ways to generate random numbers in C#](https://www.c-sharpcorner.com/article/different-ways-to-generate-random-numbers-in-C-Sharp/)