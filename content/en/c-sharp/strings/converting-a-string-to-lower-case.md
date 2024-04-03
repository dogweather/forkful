---
date: 2024-01-20 17:38:15.002505-07:00
description: 'How to: In C#, you can convert a string to lowercase using the `ToLower()`
  or `ToLowerInvariant()` methods. Here''s how.'
lastmod: '2024-03-13T22:45:00.075792-06:00'
model: gpt-4-1106-preview
summary: In C#, you can convert a string to lowercase using the `ToLower()` or `ToLowerInvariant()`
  methods.
title: Converting a string to lower case
weight: 4
---

## How to:
In C#, you can convert a string to lowercase using the `ToLower()` or `ToLowerInvariant()` methods. Here's how:

```C#
string originalText = "Hello, World!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText); // Prints: hello, world!
```

And for culture-invariant conversions:

```C#
string mixedCaseText = "İstanbul";
string lowerInvariantText = mixedCaseText.ToLowerInvariant();

Console.WriteLine(lowerInvariantText); // Prints: i̇stanbul
```

Sample output:

```
hello, world!
i̇stanbul
```

## Deep Dive
Historically, the need to convert strings to lowercase stems from computer systems that started with case-insensitive commands. Nowadays, we still do this for three main reasons:

1. **Consistency**: When treating inputs, especially user-generated data, converting to lowercase ensures a standardized format.
2. **Case-Insensitive Operations**: This includes searching, sorting, and comparing strings where "Apple" should be treated the same as "apple".
3. **Localization**: Languages have different rules for casing. `ToLowerInvariant()` addresses this by providing a culture-independent conversion, turning characters to lowercase based on invariant culture (akin to English) and avoiding unexpected results.

Alternatives to `.ToLower()` and `.ToLowerInvariant()` include using regular expressions for replacements or manually iterating through a string for custom conversion scenarios.

Implementation detail wise, these methods do not modify the original string; strings in .NET are immutable. They create and return a new string that is the lowercase version of the original.

## See Also
- String Class in C# Documentation: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string)
- StringComparison Enum and Culture-Invariant Comparisons: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
