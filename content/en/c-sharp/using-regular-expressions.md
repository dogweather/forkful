---
title:                "Using regular expressions"
html_title:           "C# recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions, or regex, are a tool used in programming to search and manipulate text based on a pattern-matching algorithm. It allows programmers to efficiently search for specific patterns within a string, helping with tasks such as data validation and parsing. Regular expressions are popular among programmers because they offer a concise and powerful way to manipulate text, making it an essential tool in many programming languages.

## How to:

Using regular expressions in C# is simple and straightforward. First, you need to define the pattern you want to search for using special syntax. For example, if you want to find all email addresses in a string, your pattern might be ```\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b```. Then, you can use the ```Regex.Match()``` method to find the first match in the string or the ```Regex.Matches()``` method to find all matches. Finally, you can use the ```Match.Value``` property to access the matched text. Here's an example in code:

```
string text = "Hello, my email is example@email.com";
string pattern = @"\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b";
Match match = Regex.Match(text, pattern);
Console.WriteLine(match.Value); // output: example@email.com
```

## Deep Dive:

Regular expressions were first developed by computer scientist Stephen Kleene in the 1950s and were popularized in the 1970s by computer scientist Ken Thompson. They have become a fundamental tool in text processing and are supported by many programming languages, including C#. There are alternatives to using regular expressions, such as string manipulation methods, but they are often more verbose and less efficient. Regular expressions offer a standardized and effective way to manipulate text and are widely used in fields such as web development, data science, and natural language processing.

In C#, regular expressions are implemented through the ```Regex``` class in the .NET framework. This class provides various methods for matching patterns, splitting strings, and replacing text. Additionally, C# provides the ```RegexOptions``` enum for specifying options such as case sensitivity and single-line matching. There are also many online resources and tools available for testing and learning regular expressions, such as [[regexr.com]] and [[regex101.com]].

## See Also:

- [[C# Regex Class Documentation|https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1]]
- [[Regular Expressions 101 Tutorial|https://www.regular-expressions.info/tutorial.html]]
- [[Mastering Regular Expressions Book|https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/]]