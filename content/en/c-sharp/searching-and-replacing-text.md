---
title:                "Searching and replacing text"
html_title:           "C# recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, especially when dealing with large amounts of data or making changes to code. It can save time and effort by making it easier to make multiple changes at once.

## How To
To perform a search and replace in C#, we can use the `Replace()` method from the `String` class. Here are some examples:

```C#
string text = "Hello world!";
string newText = text.Replace("world", "universe");

Console.WriteLine(newText); // Output: Hello universe!
```

We can also use the `Regex.Replace()` method to perform a search and replace using regular expressions.

```C#
string text = "The quick brown fox jumps over the lazy dog.";
string regex = @"[aeiou]";
string newText = Regex.Replace(text, regex, "-");

Console.WriteLine(newText); // Output: Th- q--ck br-wn f-x j-mps -v-r th- l-zy d-g.
```

## Deep Dive
When using the `Replace()` method, we can specify additional parameters such as the starting index and the number of characters to replace. This allows for more control over the replacement process.

Another important thing to keep in mind is that `Replace()` is case-sensitive. To perform a case-insensitive search and replace, we can use the `StringComparison` parameter and pass in `IgnoreCase` as an argument.

Regular expressions also offer a wide range of options for searching and replacing text. By using capturing groups and replacement patterns, we can perform complex replacements easily.

## See Also
- [String.Replace Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [Regex.Replace Method (System.Text.RegularExpressions)](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [Regular Expressions in C# (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions-in-csharp)