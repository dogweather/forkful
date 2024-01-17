---
title:                "Searching and replacing text"
html_title:           "PowerShell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is the process of finding specific words or strings within a document or file and replacing them with other text. Programmers often use this technique to make changes in a large amount of code or data with minimal effort, as it can be much more efficient than manually editing each instance.

## How to:
To search and replace text in PowerShell, we will be using the ```-replace``` operator. This operator allows us to specify the text we want to replace, as well as the replacement text. Let's look at an example:

```
$myString = "Hello World"
$myString -replace "World", "Universe"
```

In this code, we first declare a variable, ```$myString```, with the value of "Hello World". Then, we use the ```-replace``` operator to search for the string "World" within the variable and replace it with "Universe". The output of this code would be: "Hello Universe".

We can also use regular expressions with the ```-replace``` operator to search for more complex patterns within a string. Here's an example:

```
"Hello 123 World" -replace "[\d]+", "456"
```

In this code, we are replacing any sequence of numbers with the number 456. The output would be: "Hello 456 World".

## Deep Dive:
The ability to search and replace text has been a fundamental tool for programmers since the early days of computing. Prior to the invention of high-level programming languages and text editors, searching and replacing text was done manually using typewriters or printing presses.

There are also alternative methods for searching and replacing text, such as using the ```Replace()``` method in C# or using regular expressions in other languages like Python or Perl. However, PowerShell's ```-replace``` operator offers a simple and efficient solution for text manipulation within the PowerShell environment.

Under the hood, the ```-replace``` operator uses the .NET Regular Expression (Regex) engine, which is a powerful tool for matching and manipulating text based on patterns. This allows for more complex text replacements and enhances the flexibility of this feature in PowerShell.

## See Also:
To learn more about the ```-replace``` operator and regular expressions, check out the following resources:

- [About Regular Expressions - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [PowerShell Basics: Searching and Replacing Text - Adam the Automator](https://adamtheautomator.com/powershell-replace/)
- [PowerShell One-Liners: Operators - TechNet Magazine](https://technet.microsoft.com/en-us/library/ff730947.aspx)