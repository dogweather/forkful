---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co & Dlaczego?)
Capitalizing a string means converting the first character of each word to uppercase. Programmers do it for formatting output, making titles, or simply because some data standards demand it.

## How to: (Jak to zrobić:)
To capitalize a string in C#, you can use the built-in `TextInfo` class from the `System.Globalization` namespace or roll out your custom method. Here's how you can do it:

```C#
using System;
using System.Globalization;

public class Program
{
    public static void Main()
    {
        string originalText = "warszawa jest stolicą polski";
        
        // Using CultureInfo
        TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
        string capitalizedText = textInfo.ToTitleCase(originalText);
        Console.WriteLine(capitalizedText); // Output: Warszawa Jest Stolicą Polski

        // Custom method
        string CustomCapitalize(string text)
        {
            if (string.IsNullOrEmpty(text)) return text;
            return char.ToUpper(text[0]) + text.Substring(1);
        }

        string customCapitalizedText = CustomCapitalize(originalText);
        Console.WriteLine(customCapitalizedText); // Output: Warszawa jest stolicą polski
    }
}
```

## Deep Dive (W głąb tematu):
Capitalizing strings dates back to typewriters and early computers, where typography conventions transitioned into code. Today, C#'s `TextInfo.ToTitleCase` is great but not perfect; it doesn't lower the case of the rest of the words, for example.

There are two common alternatives to the `TextInfo` class:
1. Using regular expressions with `Regex.Replace` to find word boundaries and capitalize letters.
2. Implementing a custom method, which allows more control and enables you to handle edge cases, like acronyms or culture-specific rules.

Implementation wise, think about things like performance and memory usage. A custom method is potentially leaner, but the `TextInfo` approach—a product of many developers’ work—is often more robust and handles many language-specific quirks right out of the box.

## See Also (Zobacz również):
- Microsoft Documentation on `TextInfo.ToTitleCase`: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase
- Stack Overflow discussion on string capitalization in C#: https://stackoverflow.com/questions/4135317/make-first-letter-of-a-string-upper-case-with-maximum-performance
- In-depth exploration of string manipulation performance in C#: https://dotnetcoretutorials.com/2020/07/07/performance-of-string-manipulation-in-csharp/
