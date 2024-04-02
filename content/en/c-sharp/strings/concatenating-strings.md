---
date: 2024-01-20 17:34:24.305814-07:00
description: "Concatenation is the process of sticking strings together end-to-end.\
  \ We do it because often we need to combine words or symbols to create phrases,\u2026"
lastmod: '2024-03-13T22:45:00.080717-06:00'
model: gpt-4-1106-preview
summary: "Concatenation is the process of sticking strings together end-to-end. We\
  \ do it because often we need to combine words or symbols to create phrases,\u2026"
title: Concatenating strings
weight: 3
---

## What & Why?

Concatenation is the process of sticking strings together end-to-end. We do it because often we need to combine words or symbols to create phrases, messages, or compute dynamic values into readable text.

## How to:

Concatenating strings in C# can be done in several ways:

Using `+` operator:
```C#
string hello = "Hello";
string world = "World";
string concatenated = hello + ", " + world + "!";
Console.WriteLine(concatenated); // Output: Hello, World!
```

Using `String.Concat()` method:
```C#
string concatenated = String.Concat("Hello", ", ", "World", "!");
Console.WriteLine(concatenated); // Output: Hello, World!
```

Using `StringBuilder` for efficiency in loops:
```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(", ");
sb.Append("World");
sb.Append("!");
Console.WriteLine(sb.ToString()); // Output: Hello, World!
```

Using string interpolation (C# 6.0 and above):
```C#
string world = "World";
string concatenated = $"Hello, {world}!";
Console.WriteLine(concatenated); // Output: Hello, World!
```

## Deep Dive

String concatenation isn't new; it's been around since the early days of programming. However, the way we do it in C# has evolved. Originally, `+` was widely used, but it's not always efficient, especially within loops, because strings in .NET are immutable. Each `+` operation creates a new string, which can lead to performance issues.

`String.Concat()` is a direct method call that's also not loop-friendly but fine for a known, small number of strings.

`StringBuilder` is the go-to for loop scenarios or when building a string incrementally. Under the hood, `StringBuilder` maintains a buffer to accommodate additions without creating new strings for each append operation.

String interpolation, introduced in C# 6.0, allows for more readable and maintainable code. It translates into a `String.Format()` call at compile time but is easier on the eyes and less prone to errors.

Each method has its place: quick concatenations (`+`), combining a few strings (`String.Concat()`), heavy-duty string building (`StringBuilder`), and clean, formatted strings (string interpolation).

## See Also

- Microsoft Docs on String concatenation: [String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Microsoft Docs on `StringBuilder`: [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
