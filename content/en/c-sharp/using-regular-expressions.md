---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Tackling Regular Expressions in C#

## What & Why?

Regular expressions (regex) are powerful tools used for pattern matching and manipulation of strings. Programmers use regex to cut time on mundane tasks, to validate input, and to transform data.

## How to:

To leverage the power of regex in C#, we use the `System.Text.RegularExpressions` namespace. Let's look at a few examples. Remember to include the namespace by using `using System.Text.RegularExpressions;`.

#### Matching:
To check if a pattern exists in a text, we use `Regex.IsMatch(string, pattern)` function.
```C#
string test = "Hello4123";
Console.WriteLine(Regex.IsMatch(test, @"\d")); //Outputs: True
```
Here, `\d` is the pattern that matches any digit. The output is `True` because '4123' contains digits.

#### Replacing:
To replace the occurrences of a pattern with a specified string, we use `Regex.Replace(string, pattern, replacement)` function.
```C#
string test = "Hello4123";
Console.WriteLine(Regex.Replace(test, @"\d", "*")); //Outputs: Hello****
```
Again, `\d` is the pattern that matches any digit. The function replaces every digit with '*'. So, the output is "Hello****".

## Deep Dive:

The theory of regular expressions originated in the 1950s, from the work of mathematical logician Stephen Cole Kleene. The application of regex in programming languages really boomed with the advent of Perl in the 1980s.

While powerful, regex can be complex and tricky to optimize. C# provides alternatives such as `String.Contains`, `String.StartsWith`, and `String.EndsWith` for simple scenarios.

Implementation-wise, C# regex employs a backtracking algorithm for pattern matching. Careful crafting of expressions is needed to avoid excessive backtracking, which can slow down your app.

Regex also supports various options like case-insensitive (`RegexOptions.IgnoreCase`), multiline (`RegexOptions.Multiline`), etc., further increasing its versatility.


## See Also
- [MSDN Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-framework-4.8)
- [Learn Regex The Hard Way](https://regex.learncodethehardway.org/book/)
- [Regexr - An online tool to learn, build, & test Regular Expressions](https://regexr.com/)
- [Regex Crossword – a crossword puzzle with regular expressions](https://regexcrossword.com/)

Tap into regex's potential to become a more productive C# coder. The flexibility and power make them an indispensable tool in your coding toolkit.