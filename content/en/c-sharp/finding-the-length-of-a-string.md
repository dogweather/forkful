---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Finding the Length of a String in C#

## What & Why?

Finding the length of a string involves counting the number of characters it contains. Programmers use this common task to validate input, manage text data, and control program flow through conditions.

## How to:

Finding the string length in C# is straightforward, thanks to the built-in `Length` property of the `string` class.

```C#
string example = "Hello World!";
int length = example.Length;
Console.WriteLine(length); 
```

This will output:

```C#
12
```
That's it. Simple and straight to the point.

## Deep Dive

Originally, string lengths were counted manually using loops. With the introduction of high-level languages like C#, such tasks got simplified through built-in properties and methods.

As an alternative to using `Length`, you can use the LINQ `Count()` extension method:

```C#
string example = "Hello World!";
int length = example.Count();
Console.WriteLine(length);
```

Please note, `Length` property is faster and more efficient as it merely returns a precomputed integer value, while `Count()` enumerates the string every time it's called.

Under the hood, the `Length` property of a string in C# returns the value of a private field within the `System.String` class. Also, it is important to consider Unicode characters, represented as two `char` values in C#, while measuring string length. 

## See Also:

For more details on working with strings in C#, you can refer to these sources: 

1. [Strings (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
2. [String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length) in the .NET API browser