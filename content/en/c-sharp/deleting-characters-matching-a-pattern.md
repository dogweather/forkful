---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern means identifying sequences of characters that follow a predefined pattern, and then getting rid of them. Programmers do this as a part of string manipulation, which could solve problems like filtering out unwanted data, reducing noise in the text data, etc.

## How to:
Let's say you want to remove all non-alphanumeric characters from a string in C#. It's a cinch with regular expressions. Check this out:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string str = "H!e,l>l/o: W$o#r&l@d;";

        // Using Regex.Replace to get rid of non-alphanumeric characters.
        string cleanedStr = Regex.Replace(str, @"[^a-zA-Z0-9\s]", "");

        Console.WriteLine(cleanedStr);  // Will Output: 'Hello World'
    }
}
```

## Deep Dive
Historically, C# developers often used loops to manually inspect each character of a string. But as .NET evolved, `Regex.Replace` has become a more powerful tool, handling complex patterns efficiently. 

An alternative to `Regex`, particularly for simpler patterns, is using `string.replace()`. This method works fine for removing known, specific characters, but lacks the expressive power and versatility of regular expressions.

Internally, when `Regex.Replace()` executes, it matches all sequences that conform to the provided pattern and replaces them with the specified string (empty, in our case). It's important to keep in mind that `Regex.Replace()` creates a new string (since strings are immutable in C#) and potentially impacts performance if overused.

## See Also
For more insights and to deepen your understanding, refer to these resources:

1. [Microsoft Documentation on Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
2. [C# String Manipulation](https://learn.microsoft.com/en-us/dotnet/csharp/how-to/modify-string-contents)
3. [Regex tutorial — A quick cheatsheet by examples](https://www.freecodecamp.org/news/regex-tutorial-a-simple-cheatsheet-by-examples/)