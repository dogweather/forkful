---
title:                "Generating random numbers"
date:                  2024-01-20T17:48:33.921460-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Random numbers are crucial for anything unpredictable, from gaming to simulations. Coders use them to inject variety and model randomness in programs.

## How to:

Let's roll. To whip up a random number in C#, you'll need `System;` and a `Random` object.

```C#
using System;

public class RandomExample
{
    static void Main()
    {
        Random rnd = new Random();
        int randomInt = rnd.Next(1, 101); // 1 to 100
        Console.WriteLine(randomInt);
    }
}
```

Output:
```
// A number between 1 and 100, like: 47
```

For a double, it's like this:

```C#
double randomDouble = rnd.NextDouble();
Console.WriteLine(randomDouble);
```

Output:
```
// A number between 0.0 and 1.0, such as: 0.730943215
```

## Deep Dive

Historically, getting randomness on a computer, which loves predictability, was tough. Early methods were basic and pattern-prone. C# simplifies it with the `System.Random` class but remember it's pseudo-random, based on algorithms.

Alternatives? For cryptographically secure numbers, meet `System.Security.Cryptography.RNGCryptoServiceProvider`. It's heavyweight but random as it gets in C#.

Implementation? `System.Random` uses a seed. The default is the system clock, which can lead to similar numbers if instantiated rapidly. Go for custom seeds if you need.

## See Also

- Microsoft's docs on `Random`: [Random Class (System)](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- Serious randomness with `[RNGCryptoServiceProvider](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netframework-4.8)