---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# String Concatenation in C#: An Understated Powerhouse

## What & Why?
String concatenation, simply put, is the process of joining two or more strings into one. It's a fundamental tool in a programmer's utility belt, used for tasks from data formatting to output generation.

## How to:

Combining strings in C# is disarmingly easy. Check the code block below:

```C#
//Declaring the strings
string str1 = "Hello, ";
string str2 = "World!";
//Concatenating the strings
string str3 = str1 + str2;
Console.WriteLine(str3);  //Outputs "Hello, World!"
```

Just use the `+` operator. Simple, isn't it? And yes, you can chain multiple strings together:

```C#
string result = "The " + "quick " + "brown " + "fox";
Console.WriteLine(result);  //Outputs "The quick brown fox"
```

And of course, there's the `Concat` method for a more conventional approach:

```C#
string result = String.Concat("The ", "quick ", "brown ", "fox");
Console.WriteLine(result);  //Outputs "The quick brown fox"

```

## Deep Dive

Though string concatenation seems simple on the surface, there's quite an interesting history and set of complexities underneath.

Initially, concatenation performance was a concern in .NET. Each `+` operation created a new string, leaving the old ones for Garbage Collection. This wasn't optimal.

Enter StringBuilder: Introduced in .NET 1.0, it boasted better performance by reducing the need for memory allocation. If you're stringing together lots of content, especially in loops, consider StringBuilder.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("The ");
sb.Append("quick ");
sb.Append("brown ");
sb.Append("fox");
Console.WriteLine(sb.ToString());  //Outputs "The quick brown fox"
```

Another alternative is `String.Format`, which provides a handy way to insert values into a string:

```C#
string name = "John";
string greeting = String.Format("Hello, {0}!", name);
Console.WriteLine(greeting);  //Outputs "Hello, John!"
```

String interpolation, introduced in C# 6.0, has become a fan-favorite for its readability:

```C#
string name = "John";
string greeting = $"Hello, {name}!";
Console.WriteLine(greeting);  //Outputs "Hello, John!"
```

When concatenating, consider performance implications and the readability of your code. Sometimes using '+' is okay, other times StringBuilder or interpolation may be better.

## See Also

- More on StringBuilder: [Microsoft Docs - StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- More on String Interpolation: [Microsoft Docs - String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- More on Strings in C#: [Microsoft Docs - String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)