---
title:                "Capitalizing a string"
html_title:           "C# recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Title: Capitalizing Strings in C#: A Compact Guide for Developers 

## What & Why?
Capitalizing a string in programming refers to converting its first character to uppercase while leaving the rest in lowercase. Programmers use this to format text for display, validate user input, or meet syntactic requirements in coding languages.

## How to:
Here's a simple code snippet that shows how to capitalize a string in C#. We're using the `TextInfo.ToTitleCase()` method from `System.Globalization`.

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        TextInfo textInfo = new CultureInfo("en-US", false).TextInfo;
        string lowercase = "hello world!";
        string capitalized = textInfo.ToTitleCase(lowercase);

        Console.WriteLine(capitalized);  // Outputs: Hello World!
    }
}
```

## Deep Dive
Historically, the need for capitalization functions came from programming's roots in English, where capitalization varies depending on the context.

There are a few alternatives to `TextInfo.ToTitleCase()`. For example, you can use LINQ:

```C#
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        string lowercase = "hello world!";
        string capitalized = string.Concat(lowercase.Take(1).ToUpper().Concat(lowercase.Skip(1)));

        Console.WriteLine(capitalized);  // Outputs: Hello world!
    }
}
```

`TextInfo.ToTitleCase()` ignores already uppercase letters, while the LINQ alternative capitalizes only the first character, altering the rest to lower. Choose the method that suits your specific needs.

## See Also
For more details on methods used:

1. [`TextInfo.ToTitleCase()`](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
2. [LINQ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
3. [Globalization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/) in .NET

This guide was a crash course on capitalizing strings. Explore related topics for more depth!