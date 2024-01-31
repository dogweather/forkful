---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string in programming means making all letters in a string uppercase. It's a common task for formatting output, improving readability, or preparing data for comparison or storage consistency.

## How to:

In C#, you can capitalize a string using the `ToUpper` method on a string instance. Here's how it looks:

```C#
string original = "hello world!";
string capitalized = original.ToUpper();

Console.WriteLine(capitalized); // Output: HELLO WORLD!
```

Simple as that – your string is now shouting at you in all caps.

## Deep Dive

Capitalizing isn't a modern invention. In fact, old manuscripts often started with large, decorative capitulum, or capital letters. Fast forward to computing: capitalizing serves practical roles, such as making titles stand out or ensuring case-insensitive comparisons.

While `.ToUpper()` is straightforward, be aware of alternatives and quirks:

1. **Culture Sensitivity**: By default, `ToUpper()` uses the casing rules of the current culture. If you need a culture-invariant result, use `ToUpperInvariant()`.

2. **Performance**: Repeatedly capitalizing strings can be costly, especially in loops. Keep an eye out for unnecessary conversions.

3. **Alternatives**: There's also `ToLower()`, for the opposite effect (making a string all lowercase), and `TextInfo.ToTitleCase()`, for capitalizing just the first letter of each word.

4. **Security Practices**: Be cautious about transformations with security implications. For example, password comparisons should always be case-sensitive to maintain complexity.

Here's how you'd capitalize while being culture-invariant:

```C#
string original = "iççe";
string capitalizedInvariant = original.ToUpperInvariant();

Console.WriteLine(capitalizedInvariant); // Output: İÇÇE
```

Note that the dot over the 'i' remains after capitalizing per invariant culture rules.

## See Also:

- Microsoft's official documentation on `.ToUpper()`:
  [MSDN - String.ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
  
- Introduction to CultureInfo:
  [MSDN - CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)

- Best practices for using strings in .NET:
  [MSDN - Best Practices for Using Strings in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
