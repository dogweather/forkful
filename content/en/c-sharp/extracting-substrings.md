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

## Why

Substrings are an important concept in programming, especially in C#. They allow us to extract specific parts of a string, which can be useful in various scenarios such as data manipulation, data validation, and more. In this article, we will discuss the basics of extracting substrings in C# and see how they can be applied in practical situations.

## How To

To extract a substring in C#, we use the `Substring()` method. This method takes in two parameters: the starting index and the length of the substring. Let's see an example:

```C#
string sentence = "Hello world!";
string substring = sentence.Substring(6, 5);

Console.WriteLine(substring);
// Output: world
```

In the above code, we have a `sentence` string and we want to extract the word "world" from it. The first parameter in the `Substring()` method is the starting index, which in this case is 6 because "world" starts at index 6 in the string. The second parameter is the length of the substring, which is 5 because "world" has 5 characters.

We can also use string indexes instead of a length to extract a substring. For example:

```C#
string sentence = "Hello world!";
string substring = sentence.Substring(6);

Console.WriteLine(substring);
// Output: world!
```

In this code, we have only specified the starting index and not the length. This will extract all the characters from the starting index to the end of the string.

We can also use the `Substring()` method on a character array instead of a string. Let's see an example:

```C#
char[] characters = { 'H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '!' };
string sentence = new string(characters);
string substring = sentence.Substring(6, 5);

Console.WriteLine(substring);
// Output: world
```

In this code, we first convert the character array to a string using the `new` keyword, and then we use the `Substring()` method on the string.

## Deep Dive

Behind the scenes, the `Substring()` method uses the `StringBuilder` class to create a new string with the specified substring. It also performs some checks to make sure that the starting index and length are within the boundaries of the string to avoid any errors.

It's worth noting that the `Substring()` method does not modify the original string. Instead, it creates a new string with the extracted substring. This means that if we modify the extracted substring, it will not affect the original string.

Another important thing to keep in mind is that the `Substring()` method is case-sensitive. This means that if we pass in a different case string for the starting index, it will not return the expected substring.

## See Also

- [C# String.Substring Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [C# StringBuilder Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)