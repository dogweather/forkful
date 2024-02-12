---
title:                "Interpolating a string"
aliases:
- /en/c-sharp/interpolating-a-string/
date:                  2024-01-20T17:50:17.096727-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you build strings using embedded expressions. It makes code readable and formatting a breeze.

## How to:
```C# 
string name = "Alex";
int age = 29;
string greeting = $"Hello, {name}! You are {age} years old.";
Console.WriteLine(greeting);
```
Output:
```
Hello, Alex! You are 29 years old.
```

## Deep Dive
String interpolation was introduced in C# 6, bolstering the ease of formatting strings beyond the older `String.Format` method. Historically, you might've seen something like this:

```C# 
string greeting = string.Format("Hello, {0}! You are {1} years old.", name, age);
```

Interpolation in C# is a syntactic sugar that the compiler converts into a `String.Format` call. It works by parsing the interpolated string and replacing the expressions enclosed in `{}` with the string representations of the expressions' results. Internally, it uses a `StringBuilder` under the hood, making it more efficient than concatenation in loops.

Alternative to string interpolation is the plus (`+`) operator for concatenation, but that can quickly become unreadable and cumbersome, and often more error-prone.

```C# 
string greeting = "Hello, " + name + "! You are " + age + " years old.";
```

Given these alternatives, string interpolation is often the preferred choice for its clarity and efficiency in most scenarios.

## See Also
For more on string formatting in C#, MSDN is your pal:
- [String interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
- [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
