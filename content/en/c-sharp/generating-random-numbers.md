---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Random Numbers in C#: Make Your Code Unpredictable

## What & Why?
Generating random numbers refers to producing a sequence of numbers that lack any pattern. Programmers employ this to introduce unpredictability where needed—think games, simulations, and even security systems.

## How to:
C# makes it pretty straightforward to create random numbers through its built-in `Random` class.

Here's a basic example.
```C#
Random rand = new Random();
int randNum = rand.Next(); // Generates a non-negative random integer
Console.WriteLine(randNum);
```
When you run this code, you'll see a different random integer every time.

If you want your random number within a certain range, you just need to specify the min (inclusive) and max values (exclusive).
```C#
Random rand = new Random();
int randNum = rand.Next(1, 101); // Generates a random integer from 1 to 100
Console.WriteLine(randNum);
```
Again, running this you'll get a different number between 1 and 100 each time.

## Deep Dive
You may wonder why C# provides a `Random` class and not just a simple random function like some other languages (hint: JavaScript). That's largely historical - `Random` first appeared in .NET Framework 1.1 and has been part of C# since its inception.

As for alternatives, well, your options are vast. Other classes like `RNGCryptoServiceProvider` can be a better choice for scenarios requiring more robust randomness such as in cryptography. However, it's more complex and slower—so it's all about finding the right tool for your use case.

In terms of implementation, the `Random` class in C# uses a time-dependent seed value—more specifically, the current system time. This is why you should avoid creating multiple `Random` instances too quickly or at the same time. Because they'd all be seeded with the same system time, you'd end up with the same "random" numbers—a situation that is decidedly un-random!

## See Also
For more details and discussions, feel free to dive into the official Microsoft documentation pages: 

- [Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [RNGCryptoServiceProvider Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)

Also, StackOverflow has some great threads around best practices when working with random numbers in C#: 

- [C# Random Number Generation](https://stackoverflow.com/questions/1785744/how-do-i-seed-a-random-class-to-avoid-getting-duplicate-random-values)
- [Cryptography StackOverflow](https://stackoverflow.com/q/1499269)