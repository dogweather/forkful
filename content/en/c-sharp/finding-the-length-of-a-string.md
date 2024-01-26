---
title:                "Finding the length of a string"
date:                  2024-01-20T17:47:08.737490-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding a string's length means counting its characters. We do it to validate input, loop through characters, allocate resources, or simple curiosity – knowing the size matters.

## How to:

In C#, the `string.Length` property gives you the number of characters in a string. Here's how to use it:

```C#
using System;

class Program
{
    static void Main()
    {
        string example = "Hello, World!";
        Console.WriteLine(example.Length); // Output: 13
    }
}
```

Easy, right? But remember, it counts *characters*, not bytes. With emojis or special characters, things can get tricky. More on that later.

## Deep Dive

Historically, finding the length of a string was tied to memory management and manipulation in programming. Since C# is a high-level language, it abstracts that low-level work away. Still, it's good to know what's under the hood.

Alternatives? Sure! You might see `example.ToCharArray().Length` out in the wild, but it's just doing extra legwork for the same result.

Now, about those tricky characters. C#'s `Length` property counts a string's `char` objects, each representing a UTF-16 code unit. That's fine until you encounter *surrogate pairs* – characters like emojis that need two `char` objects. Here's the thing: `Length` counts those as two. Yep.

For an accurate count of *visual* characters or *grapheme clusters*, you'd need System.Globalization's `StringInfo` class:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string example = "👍"; // Thumbs up emoji

        Console.WriteLine(example.Length); // Output: 2 <- Because of the surrogate pair!
        Console.WriteLine(new StringInfo(example).LengthInTextElements); // Output: 1
    }
}
```

Understand the difference? It's not just academic; it could affect text processing in meaningful ways.

## See Also

Explore more with these resources:

- [Microsoft's official documentation on strings](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Understanding Unicode and UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html)
- [StringInfo class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo?view=net-6.0)

Know your strings, handle them wisely, and write code that counts – in every sense.
