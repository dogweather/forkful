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

## What & Why?
Converting a string to lower case simply means changing all the letters in a string to their lower case counterparts. This is a common task for programmers when they need to compare two strings without worrying about case sensitivity. It also makes the string more readable for users.

## How to:
Converting a string to lower case in C# is simple with the built-in method `ToLower()`. Here's an example:

```C#
string name = "JOHN";
string lowerCaseName = name.ToLower();
Console.WriteLine(lowerCaseName);
```

Output:
```C#
john
```

You can also use the `ToLower()` method directly on the string variable without creating a new variable:

```C#
string name = "JOHN";
Console.WriteLine(name.ToLower());
```

Output:
```C#
john
```

## Deep Dive:
In the early days of programming, creating a case-insensitive string comparison required converting both strings to either upper case or lower case. However, as programming languages evolved, the `ToLower()` method was introduced to make this task easier and more efficient.

An alternative to using `ToLower()` is the `String.Equals()` method, which has an option to ignore case when comparing two strings. However, if you need to convert a string to lower case for use in other methods, it's more convenient to use `ToLower()`.

Under the hood, the `ToLower()` method uses the CurrentCulture to determine the appropriate lower case letters. This means that the result can vary depending on the computer's language settings. If you need a consistent result, you can use the `ToLowerInvariant()` method, which always converts to lower case using the invariant culture.

## See Also:
- [String.ToLower Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [String.Equals Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.equals?view=net-5.0)
- [String.ToLowerInvariant Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-5.0)