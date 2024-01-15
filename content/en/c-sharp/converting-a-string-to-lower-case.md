---
title:                "Converting a string to lower case"
html_title:           "C# recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case can be useful for various reasons, such as making the input more consistent for comparison, improving search functionality, or simply for better readability in certain scenarios.

## How To

```C#
// Using the ToLower() method
string input = "Hello World!";
string lowerCase = input.ToLower();
Console.WriteLine(lowerCase); // Output: hello world!

// Using the ToLowerInvariant() method
string input = "Hello World!";
string lowerCase = input.ToLowerInvariant();
Console.WriteLine(lowerCase); // Output: hello world!

// Avoiding culture-specific case mappings 
// by specifying the culture as InvariantCulture
string input = "HeLlO WoRlD!";
string lowerCase = input.ToLower(CultureInfo.InvariantCulture);
Console.WriteLine(lowerCase); // Output: hello world!
```

## Deep Dive

When converting a string to lower case, it is important to understand that the result may differ depending on the current culture settings. In simple terms, the culture setting determines the language and formatting rules used for various operations in your program.

The ToLower() method uses the current culture settings to convert the string to lower case, while the ToLowerInvariant() method uses a fixed, culture-independent algorithm. This makes the ToLowerInvariant() method more suitable for scenarios where you want the same result regardless of the culture settings.

Additionally, the ToLower() method takes into consideration culture-specific mappings, which can lead to unexpected results. For example, the Turkish culture has a unique behavior where the "I" character is converted to "ı" instead of "i" when converting to lower case. This can cause issues when performing case-insensitive string comparisons.

By specifying the InvariantCulture when using the ToLower() method, you can avoid these culture-specific case mappings and ensure consistent results.

See Also

- [String.ToLower() Method (System) | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [String.ToLowerInvariant() Method (System) | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- [Culture-Specific Case Mappings in String Operations | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/culture-specific-case-mappings)