---
title:                "Extracting substrings"
html_title:           "C# recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings in programming means taking out a specific portion or section of a longer string. Programmers often do this to manipulate or access certain portions of a string, such as extracting a username from an email address or retrieving a certain value from a larger dataset. It helps to make code more efficient and organized.

## How to:

### Basic method

To extract a substring in C#, you can use the ```Substring()``` method. This method takes in two parameters - the starting index of the substring and the length of the substring.

```
string myString = "Hello World";
string subString = myString.Substring(6, 5);
```

In this example, we want to extract the word "World" from the string "Hello World". We specify the starting index as 6 (corresponding to the first character of "World") and the length as 5 (the number of characters in "World"). The output will be the substring "World".

### Advanced method using Index and Range

Starting with C# 8.0, we can use the ```[..]``` index and range operators to extract substrings in a more concise way. The square brackets are used to specify the start and end indices of the substring we want.

```
string myString = "Hello World";
string subString = myString[6..];
```

This will extract the substring starting from index 6 (corresponding to the first character of "World") to the end of the string. The output will be "World".

## Deep Dive

The ```Substring()``` method was first introduced in .NET Framework 1.1 and has also been available in subsequent versions. However, the use of indexes and ranges for extracting substrings is a newer feature, introduced in C# 8.0.

There are other ways to extract substrings in C#, such as using the ```Remove()``` method or the ```Split()``` method. However, the use of the ```Substring()``` method or the index and range operators are the most commonly preferred methods as they offer more flexibility and clarity.

It is also important to note that extracting substrings can impact performance, especially when dealing with large strings or using nested substring operations. Care should be taken to choose the most efficient method based on the specific needs of the program.

## See Also

- [Official Microsoft documentation for the Substring method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Explanation of index and range operators in C#](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-8#indices-and-ranges)