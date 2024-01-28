---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:20.447122-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in C# involves creating unpredictable values that can be used for a variety of purposes, such as simulations, games, or security applications. Programmers utilize random number generation to introduce non-determinism into their software, making behaviors less predictable and more dynamic.

## How to:

In C#, the `System.Random` class is commonly used to generate random numbers. Here's how to instantiate it and generate some basic random values:

```C#
using System;

public class RandomDemo
{
    public static void Main(string[] args)
    {
        // Create an instance of Random class
        Random randomGenerator = new Random();

        // Generate a random integer between 0 and 100
        int randomInt = randomGenerator.Next(0, 101);
        Console.WriteLine($"Random Integer: {randomInt}");

        // Generate a random double
        double randomDouble = randomGenerator.NextDouble();
        Console.WriteLine($"Random Double: {randomDouble}");

        // Generating a random byte array
        byte[] buffer = new byte[5];
        randomGenerator.NextBytes(buffer);
        Console.Write("Random Bytes: ");
        foreach (var b in buffer)
        {
            Console.Write($"{b} ");
        }
    }
}
```

Sample output might look like this:
```
Random Integer: 42
Random Double: 0.843196
Random Bytes: 204 155 3 18 67
```

Keep in mind that each instance of `Random` is initialized with a time-dependent seed value. Creating multiple `Random` instances in quick succession can result in the same "random" numbers.

## Deep Dive

The `System.Random` class in C# implements a pseudo-random number generator (PRNG), which means the numbers it generates are not truly random but are deterministic and can be reproduced if the seed value is known. Historically, this has been sufficient for many applications not requiring cryptographic security. However, for scenarios where security is paramount, such as generating encryption keys or tokens, `System.Security.Cryptography.RandomNumberGenerator` class is a more suitable choice as it generates cryptographically secure random numbers.

While `System.Random` is easy and quick for generating random numbers for simulations, testing, and games, when working with .NET, it's essential to choose the right tool for the job. The introduction of `System.Security.Cryptography.RandomNumberGenerator` acknowledges the importance of providing developers with a secure means of generating randomness, particularly in an era where digital security is paramount.

Moreover, advancements in computer science and cryptography continuously inform the evolution and improvement of random number generation in coding languages, including C#. As such, it's crucial for developers to stay updated on best practices and emerging tools to ensure their applications remain secure, efficient, and effective.

## See also

### Official .NET Documentation
- [Random Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)

### Tutorials and Guides
- **TutorialsTeacher**: [C# - Random Numbers](https://www.tutorialsteacher.com/csharp/csharp-random)
- **C# Corner**: [Generating Random Numbers in C#](https://www.c-sharpcorner.com/article/generating-random-numbers-in-C-Sharp/)

### Online Courses and Videos
- **Udemy**: [Complete C# Masterclass](https://www.udemy.com/course/complete-csharp-masterclass/) *(Relevant section on random numbers)*
- **YouTube**: [Random Class in C# | Random Numbers](https://www.youtube.com/watch?v=tw7ror9x32s)

### Forums and Community Discussions
- **Stack Overflow**: [How to generate a random int in C#](https://stackoverflow.com/questions/767999/random-number-generator-only-generating-one-random-number)
- **Reddit - r/csharp**: [Best practices for Random number generation in C#](https://www.reddit.com/r/csharp/comments/jb0s7k/best_practices_for_random_number_generation_in_c/)
