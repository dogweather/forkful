---
title:                "C# recipe: Converting a string to lower case"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why 

Converting a string to lower case is a common task in programming, especially when dealing with user input or manipulating strings for various purposes. It allows for easier comparison and consistency in the code. 

## How To 

To convert a string to lower case in C#, we can use the `ToLower()` method. This method is available for all strings and returns the string in lower case letters. Let's take a look at an example:

```C#
string name = "John Smith";
string nameLower = name.ToLower();
Console.WriteLine(nameLower);
```
Output: john smith

In the above code, we first initialize a string variable with the value "John Smith". Then, we call the `ToLower()` method on that string and store the result in another variable. Finally, we use `Console.WriteLine()` to print out the lower case version of the string.

We can also use the `ToLower()` method directly in a `Console.WriteLine()` statement, without creating an extra variable:

```C#
Console.WriteLine("Hello World".ToLower());
```
Output: hello world

If we want to convert a string to lower case while also ignoring any existing cultural settings or case rules, we can use the `ToLowerInvariant()` method instead.

## Deep Dive 

Under the hood, the `ToLower()` method uses the `TextInfo.ToLower()` method, which in turn uses a combination of the `ToLowerCaseChar()` and `ToLowerAsciiChar()` methods to perform the conversion. The `TextInfo` class is part of the `System.Globalization` namespace and provides culture-specific information that is used in string manipulation tasks.

One thing to keep in mind when converting a string to lower case is that certain cultures might have special case mappings, meaning that the output might differ depending on the language or cultural setting. 

Another useful method for converting strings to lower case is `ToLowerInvariant()`, which uses the invariant culture for the conversion. This ensures that the result is consistent regardless of the cultural setting.

## See Also 
- String.ToLower Method (https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- TextInfo.ToLower Method (https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.tolower?view=netcore-3.1)
- Understanding Exploding Strings in .NET (https://www.c-sharpcorner.com/article/understanding-exploding-strings-in-net/)