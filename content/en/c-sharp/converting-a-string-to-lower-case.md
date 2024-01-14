---
title:                "C# recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting strings to lower case may seem like a simple task, but it can actually have a big impact on the readability and functionality of your code. By converting a string to lower case, you can ensure that your input is standardized and easier to compare and manipulate. In this blog post, we'll explore how to convert a string to lower case in C# and why it can be beneficial in your programming projects.

## How To
To convert a string to lower case in C#, we can use the `.ToLower()` method. This method is available on the `string` data type and will return a new string with all characters converted to lower case. Let's see an example:

```C#
string myString = "HELLO WORLD";
string lowerCaseString = myString.ToLower();

// Output: "hello world"
Console.WriteLine(lowerCaseString);
```

We can also use the `ToLower()` method to only convert certain parts of a string. For example, if we only want to convert the first letter of a string to lower case, we can use the `Substring()` method to extract the first letter, convert it, and then concatenate it back with the rest of the string. Here's an example of how to convert the first letter of a string to lower case:

```C#
string myString = "Hello World";
string firstLetter = myString.Substring(0, 1).ToLower();
string restOfString = myString.Substring(1);
string convertedString = firstLetter + restOfString;

// Output: "hello World"
Console.WriteLine(convertedString);
```

It's important to remember that the `.ToLower()` method does not change the original string, but instead returns a new string with the converted characters. This means that if you want to use the converted string, you will need to assign it to a variable like we did in the examples above.

## Deep Dive
The `.ToLower()` method in C# uses the `TextInfo` class from the `System.Globalization` namespace to perform the conversion. This class contains methods for manipulating and comparing strings based on culture-specific conventions. By default, the `ToLower()` method will use the current culture to determine how to convert the characters to lower case. However, you can also specify a specific culture when calling the method, which can be useful if you're working with data from different cultures.

It's also worth noting that the `.ToLower()` method only works for characters in the English alphabet. If you're working with other languages, you may need to use the `.ToLowerInvariant()` method, which will perform the conversion without taking the current culture into account.

## See Also
- [TextInfo.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.tolower?view=netcore-3.1)
- [String.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Culture-Specific String Operations in C# (DZone)](https://dzone.com/articles/culture-specific-string-operations-c)