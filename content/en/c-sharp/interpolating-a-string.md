---
title:                "Interpolating a string"
html_title:           "C# recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string in C# is a useful feature that allows you to insert values into a string at runtime. It is especially helpful when building complex strings that include dynamic or changing values. Programmers use this feature for a variety of reasons, such as creating dynamic SQL queries, displaying user input, or forming complex error messages.

## How to:

Interpolating a string in C# is simple. It involves adding the `$` symbol before the string and wrapping the dynamic values in curly braces. Here's an example:

```C#
string name = "John";
string message = $"Hello, {name}!"; //output: Hello, John!
```

You can also perform operations on the dynamic values within the curly braces. For example:

```C#
int num1 = 5;
int num2 = 10;
string result = $"The sum of {num1} and {num2} is {num1 + num2}."; //output: The sum of 5 and 10 is 15.
```

## Deep Dive:

Interpolating a string was introduced in C# 6.0. It was a more concise and readable alternative to the existing `String.Format` method. Before this feature, programmers had to use placeholders and then pass the dynamic values as arguments. For example:

```C#
string name = "John";
string message = String.Format("Hello, {0}!", name); //output: Hello, John!
```

Another alternative to string interpolation is using string concatenation, which involves using the `+` operator to combine strings and values. However, this can be time-consuming and error-prone, especially when working with long and complex strings.

Internally, the C# compiler converts interpolated strings into `String.Format` calls, so both methods have similar performance. However, string interpolation is much more readable and often preferred by programmers.

## See Also:

- [C# Interactive: String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/intro-to-csharp/introduction-to-interactive?WT.mc_id=docs-github-ninad)
- [String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated?WT.mc_id=docs-github-ninad)
- [How to: Use String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated?WT.mc_id=docs-github-ninad&view=netframework-4.7.2#use-string-interpolation-in-a-custom-method)
- [Performance Comparison: Interpolated String vs. Composite Format](https://dev.to/alriksson/string-interpolation-performance-4pa8)