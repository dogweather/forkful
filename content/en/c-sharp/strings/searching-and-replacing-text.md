---
title:                "Searching and replacing text"
aliases: - /en/c-sharp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:39.428227-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text in strings lets you update data without manual edits. Programmers need this to handle user input corrections, data formatting, or batch updates efficiently.

## How to:
C# makes text manipulation pretty straightforward. Below, check out the `string.Replace` method to swap out words.

```C#
using System;

public class Program
{
    public static void Main()
    {
        string phrase = "Hello, World!";
        string updatedPhrase = phrase.Replace("World", "C#");
        
        Console.WriteLine(updatedPhrase); // Output: Hello, C#!
    }
}
```

No rocket science, right? But say we want to ignore case or replace only whole words? Regex to the rescue:

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "Apples grow on trees. apple pies are tasty.";
        string pattern = "\\bapple\\b"; // \b is a word boundary in Regex
        string replacement = "Orange";
        
        string updatedPhrase = Regex.Replace(phrase, pattern, replacement, RegexOptions.IgnoreCase);

        Console.WriteLine(updatedPhrase); // Output: Oranges grow on trees. Orange pies are tasty.
    }
}
```

## Deep Dive
Back in the day, manipulating strings was a hassle. C was all we had, and it meant dealing with character arrays and manual iterations. C# gave us a gift: easy string handling.

If `string.Replace` or `Regex.Replace` don't cut it, we've got options. For huge texts or complex patterns, consider writing a custom parser or use libraries like Antlr.

Regex is powerful for pattern matching but can be slow. If performance is critical and you're into the nitty-gritty details, measure and compare with `StringBuilder` for massive, iterative replacements.

## See Also
- Microsoft Docs on [`string.Replace`](https://docs.microsoft.com/dotnet/api/system.string.replace)
- .NET's [`Regex`](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex) class for more sophisticated patterns
- Check out Antlr for complex parsing: [The ANTLR Mega Tutorial](https://tomassetti.me/antlr-mega-tutorial/)
