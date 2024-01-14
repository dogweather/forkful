---
title:                "C# recipe: Extracting substrings"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in many programming languages, including C#. It involves extracting a specific portion of a larger string based on its starting and ending index values. This can be useful for a variety of tasks such as manipulating text, parsing data, or formatting output. By learning how to extract substrings in C#, you can increase your efficiency and productivity as a programmer.

## How To

To extract a substring in C#, you will need to use the `Substring` method. This method takes in two parameters: the starting index and the length of the substring. For example, if we have a string "Hello World", and we want to extract the word "World", we can use the following code:

```C#
string hello = "Hello World";
string world = hello.Substring(6, 5); //returns "World"
Console.WriteLine(world); //outputs "World"
```

As you can see, we specified the starting index of 6 (which corresponds to the first index of the character "W" in "World") and a length of 5 (which is the number of characters in "World").

You can also use the `Substring` method with variables instead of hardcoding the index values. For example:

```C#
string hello = "Hello World";
int start = 6;
int length = 5;
string world = hello.Substring(start, length); //returns "World"
```

This approach allows for more dynamic and flexible code.

There are also other useful string methods that can be used in conjunction with `Substring`, such as `IndexOf` and `LastIndexOf`. These methods can help you determine the correct starting index for your substring based on a specific character or string. For example:

```C#
string sentence = "I love coding in C#";
int startIndex = sentence.IndexOf("coding"); //returns 7
int length = 6; //length of "coding"
string coding = sentence.Substring(startIndex, length); //returns "coding"
```

Lastly, it's important to remember that in C#, strings are immutable, meaning they cannot be changed. So when we extract a substring, a new string is actually created and returned. This is something to keep in mind when working with large strings.

## Deep Dive

Under the hood, the `Substring` method in C# works by creating a new string object with the specified substring. This process involves allocating memory, copying characters, and creating a new object reference. While this may seem like a lot of work, it is actually quite efficient in terms of performance. The `Substring` method also handles out-of-bounds exceptions, ensuring that your code doesn't break unexpectedly.

In addition to the `Substring` method, there are also other ways to extract substrings in C#. The `Substring` method is great for extracting a portion of a string based on the index values, but if you want to extract a substring based on a specific delimiter or pattern, you can use methods such as `Split` or `Regex`. These methods provide even more flexibility for substring extraction and can be useful in specific scenarios.

## See Also

Here are some additional resources for learning about substring extraction in C#:

- C# String.Substring Method: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- C# String.Split Method: https://docs.microsoft.com/en-us/dotnet/api/system.string.split
- C# Regex Class: https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex