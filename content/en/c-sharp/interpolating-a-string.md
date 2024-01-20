---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is a convenient way to format and insert expressions into strings in C#. It increases readability and makes it easier to manage complex string layouts.

## How to:

Here's how to interpolate a string in C#:

```C#
string name = "John";
int age = 23;
string result = $"Hello, my name is {name} and I am {age} years old.";
Console.WriteLine(result);
```
And the output will be:
```
Hello, my name is John and I am 23 years old.
```

It's as simple as prefixing a string with a $ and including your variables or expressions in curly braces `{}` within that string.

## Deep Dive

Historically, string formatting in C# has been done via `String.Format` method or using concatenation. The introduction of string interpolation in C# 6 was a game-changer.

```C#
// The old way
string result = string.Format("Hello, my name is {0} and I am {1} years old.", name, age);
```

Although `String.Format` and concatenation are still valid methods, string interpolation offers more readability and less complexity, especially with complex strings and expressions.

Do note that underneath, every interpolated string is transformed into a `String.Format` by the compiler. Therefore, the performance difference is negligible.

## See Also

* [String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation)
* [Mastering C# 6.0 features](https://www.infoworld.com/article/2989972/what-is-string-interpolation-in-c-6.html)