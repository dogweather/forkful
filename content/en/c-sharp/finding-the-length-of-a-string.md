---
title:                "C# recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As developers, we often encounter situations where we need to manipulate strings in our code. One common task is to find the length of a string, which can be useful for various operations such as validation or formatting. In this blog post, we will discuss how to find the length of a string in C# and why it is important.

## How To

The first step to finding the length of a string is to declare a variable of type string and assign it a value. For example:

```C#
string myString = "Hello World";
```

Next, we can use the `Length` property of the string object to get the length of the string. This property returns an integer value representing the number of characters in the string. Let's print out this value to the console using the `Console.WriteLine` method:

```C#
Console.WriteLine("The length of the string is: " + myString.Length);
```

The output of this code would be:

```
The length of the string is: 11
```

We can also use the `Length` property in conditional statements, such as if statements, to perform different actions depending on the length of the string. For example:

```C#
if(myString.Length > 10)
{
	Console.WriteLine("String is longer than 10 characters.");
}
else
{
	Console.WriteLine("String is less than or equal to 10 characters.");
}
```

This will print out the appropriate message based on the length of the string.

## Deep Dive

Behind the scenes, the `Length` property uses the `Length` property of the `StringInfo` class to calculate the number of characters in a string. This class is part of the `System.Globalization` namespace and provides methods for working with strings in different cultures and languages.

It is also important to note that the `Length` property will return the number of UTF-16 code units in a string, not the number of characters. This might be different for certain languages where characters may be composed of multiple code units.

## See Also

- [String.Length Property - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [StringInfo Class - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo)
- [TextElementEnumerator.Length Property - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textelementenumerator.length)