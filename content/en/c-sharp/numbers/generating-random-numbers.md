---
title:                "Generating random numbers"
aliases: - /en/c-sharp/generating-random-numbers.md
date:                  2024-01-27T20:26:21.009989-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in C# involves the creation of unpredictable numeric values within a specified range. Programmers use these methods to implement features like cryptography, simulations, and games where unpredictability or the simulation of real-world randomness is required.

## How to:

The most common way to generate random numbers in C# is using the `System.Random` class. Here's a simple example demonstrating its usage:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Generates a number between 1 and 99
        Console.WriteLine($"Random number: {randomNumber}");
    }
}
```

This will output a random number such as:

```
Random number: 42
```

For generating a random floating-point number between 0.0 and 1.0, you can use the `NextDouble` method:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Random double: {randomDouble}");
```

If you're working on a security-sensitive application that requires cryptographic randomness, it's better to use the `RNGCryptoServiceProvider` class found in `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Creates a 4-byte long random number
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Cryptographically secure random number: {value}");
    }
}
```

## Deep Dive

Random number generation in C# has evolved over the years. Initially, the `System.Random` class was the go-to for generating pseudo-random numbers. It is pseudo-random because, given a specific seed value, it will produce the same sequence of numbers, which can be useful for debugging or repeatability of tests.

While sufficient for basic needs, `System.Random` is not thread-safe and can produce predictable outcomes, which is not suitable for security-dependent applications. This limitation led to the introduction of the `RNGCryptoServiceProvider` for cryptographic randomness, which is more secure but also more resource-intensive.

An alternative in .NET Core and .NET 5+ is the `RandomNumberGenerator` class in `System.Security.Cryptography` for generating random numbers securely, which is intended as a more modern and easy-to-use option compared to `RNGCryptoServiceProvider`.

Each method of generating random numbers in C# has its place depending on the requirements of the application. For most applications, `System.Random` suffices, but for those that require secure, unpredictable random numbers, the cryptographic classes provide a robust alternative.
