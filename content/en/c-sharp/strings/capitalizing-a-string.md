---
date: 2024-02-03 19:02:51.367491-07:00
description: "Capitalizing a string in C# involves converting the first character\
  \ of a string to uppercase if it isn't already. This alteration can be crucial for\u2026"
lastmod: '2024-03-11T00:14:33.936605-06:00'
model: gpt-4-0125-preview
summary: "Capitalizing a string in C# involves converting the first character of a\
  \ string to uppercase if it isn't already. This alteration can be crucial for\u2026"
title: Capitalizing a string
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string in C# involves converting the first character of a string to uppercase if it isn't already. This alteration can be crucial for formatting outputs, enforcing coding standards, or making user interface texts more readable.

## How to:
C# offers a straightforward approach to capitalizing strings using built-in methods. The simplest way to achieve this is by modifying the string directly with these methods. For more complex or specific capitalization rules (e.g., capitalizing each word), additional libraries or manual methods might be necessary. Below are examples demonstrating how to capitalize a string in various ways in C#.

### Basic Capitalization:
To capitalize the first letter of a single word or sentence:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Output: "Hello world"
```

### Capitalizing Each Word:
For capitalizing the first letter of each word in a string, you can use the `TextInfo.ToTitleCase` method found in the `System.Globalization` namespace:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Output: "Hello World"
```

Note: `ToTitleCase` does not lower the case of the rest of the letters; it only changes to uppercase the first letter of each word. Also, certain words in title case rules (like "and", "or", "of") may not be capitalized depending on the culture settings.

### Using Extension Methods for Reusability:
You can create an extension method for the `string` class to simplify the capitalization process, making your code cleaner and more reusable. Here's how to create and use such a method:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Output: "Hello world"
    }
}
```

This extension method `Capitalize` can be called on any string object within the namespace, offering a more intuitive and object-oriented approach to string manipulation in C#.

### Third-Party Libraries:
While C#'s standard library covers most needs for string capitalization, certain specialized tasks might benefit from third-party libraries, such as Humanizer. However, for the task of simply capitalizing strings or each word in a string, standard C# methods are adequate and efficient, negating the need for external dependencies.
