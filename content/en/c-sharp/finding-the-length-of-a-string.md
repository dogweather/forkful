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

## What & Why?

Finding the length of a string is a common task in programming that involves determining the number of characters or elements contained within a given string. Programmers often need to know the length of a string in order to manipulate or compare it with other strings, or to allocate the necessary amount of memory for it. 

## How To:

To find the length of a string in C#, you can use the built-in ```Length``` property of the ```String``` class. Here's an example:

```C#
string myString = "Hello World";
int length = myString.Length;
Console.WriteLine(length);
```

This will output the value ```11```, as there are 11 characters in the string "Hello World".

You can also use the ```Count``` method from the ```Enumerable``` class to find the length of a string, as shown below:

```C#
string myString = "Goodbye";
int length = Enumerable.Count(myString);
Console.WriteLine(length);
```

This will also output the value ```7```, as there are 7 characters in the string "Goodbye".

## Deep Dive:

Historically, counting the number of characters in a string was a more complicated task, as languages like C and C++ did not have built-in methods or classes for this purpose. Programmers had to write their own functions to iterate through the string and count each character, often using techniques like pointers and memory allocation to do so.

In C#, the ```Length``` property was introduced to simplify this process and provide a more efficient way to find the length of a string. However, the ```Count``` method is still a useful alternative for when you need to find the length of a sequence of elements, not just a string.

As for implementation details, the ```Length``` property returns an integer representing the number of characters in the string, including spaces and special characters. It does not count null characters (\0) or escape characters (such as \n or \t). The ```Count``` method, on the other hand, can be used with any object that implements the ```IEnumerable``` interface, making it a more versatile approach for finding the length of a sequence.

## See Also:

- Microsoft documentation on [String.Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- Microsoft documentation on [Enumerable.Count](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.count)
- [String Length Calculation in C and C++](https://www.geeksforgeeks.org/length-of-a-string-using-pointer-in-c/) by GeeksforGeeks