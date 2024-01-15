---
title:                "Finding the length of a string"
html_title:           "C# recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Strings are a fundamental type in any programming language, and a common task when working with them is finding their length. Knowing the length of a string can help with tasks such as validation, formatting, and manipulation.

## How To
Finding the length of a string in C# is a simple task that can be achieved in different ways. Let's explore two popular methods: the `Length` property and the `Count()` method.

#### Using the `Length` property
Here's a simple example of how to use the `Length` property to find the length of a string: 

```C#
string sentence = "Hello World";
int length = sentence.Length; // length = 11
Console.WriteLine("The length of the string is: " + length); // Output: The length of the string is: 11
```

The `Length` property returns the total number of characters in a string, including spaces and punctuation.

#### Using the `Count()` method
Another way to find the length of a string is by using the `Count()` method. Here's an example:

```C#
string sentence = "Hello World";
int length = sentence.Count(); // length = 11
Console.WriteLine("The length of the string is: " + length); // Output: The length of the string is: 11
```

The `Count()` method returns the number of elements in a sequence, which for a string is equivalent to its length.

## Deep Dive
Under the hood, both the `Length` property and the `Count()` method are based on the `String` class. The `Length` property is simply a property of the `String` class, while the `Count()` method is an extension method that uses the `Length` property internally.

It's also worth mentioning that the `Length` property and the `Count()` method have different performance implications. The `Length` property performs slightly better since it's a direct property of the `String` class, while the `Count()` method has to go through additional checks.

## See Also
- [C# String Handling](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length/)
- [Enumerable.Count Method](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.count)