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

## Why 
Random numbers are an essential part of many computer programs, whether it's for games, simulations, or encryption algorithms. Generating random numbers allows for unpredictability and helps add an element of randomness to programs.

## How To
Generating random numbers in C# is simple and can be done using the Random class in the System namespace. Here's a basic example:

```C#
//initialize a new instance of the Random class
Random rand = new Random(); 

//generate a random integer between 0 and 10
int randomNumber = rand.Next(0, 11); 

//output the random number
Console.WriteLine("Random number: " + randomNumber); 
```

This code creates a Random object and uses the Next() method to generate a random integer between 0 and 10. The result is then printed to the console.

Other useful methods from the Random class include NextDouble(), which generates a random double between 0.0 and 1.0, and NextBytes(), which fills an array with random bytes. It's important to note that the Random class uses a seed value to generate random numbers, so if the same seed is used, the same sequence of random numbers will be generated.

## Deep Dive
Behind the scenes, the Random class uses an algorithm called the Linear Congruential Generator (LCG) to generate random numbers. This algorithm uses a mathematical formula to produce a pseudo-random sequence of numbers. It's based on the current system time, and the formula is designed to provide a uniform distribution of numbers.

It's worth mentioning that the Random class in C# should not be used for cryptographic purposes as it's not cryptographically secure. For encryption algorithms, a cryptographically secure random number generator should be used instead.

## See Also
- Microsoft Docs on Random Class: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0 
- C# Random Numbers Tutorial: https://www.c-sharpcorner.com/article/random-number-class-in-C-Sharp/