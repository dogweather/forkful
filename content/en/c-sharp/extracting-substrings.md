---
title:                "C# recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extracting Substrings in C# is Useful

Extracting substrings is a common task in everyday programming. It involves taking a small portion of a larger string and using it for a specific purpose. This can be useful in situations where you only need a certain part of a string, such as extracting a name from a full email address or getting the day and month from a date string.

## How To Extract Substrings in C#

The process of extracting substrings in C# is fairly straightforward. Here is an example of how you can extract a substring from a string:

```C#
string fullName = "John Doe";
string firstName = fullName.Substring(0, 4); // Output: "John"
string lastName = fullName.Substring(5); // Output: "Doe"
```

In this example, we have a full name string and we want to extract the first name and last name. Using the `Substring()` method, we can specify the starting index and length of characters we want to extract. In the first line, we start at index 0 and extract 4 characters, which gives us the first name "John". In the second line, we only specify the starting index, so the `Substring()` method extracts all the characters from that index until the end of the string, giving us the last name "Doe".

We can also use the `Substring()` method to extract part of a string based on a specific character. Here's an example:

```C#
string email = "johndoe@example.com";
string username = email.Substring(0, email.IndexOf("@")); // Output: "johndoe"
```

In this example, we have an email address and we want to extract the username before the "@" symbol. We use the `Substring()` method and the `IndexOf()` method to get the index of the "@" symbol, which we then use in the `Substring()` method to extract the characters before it.

##Deep Dive into Substring Extraction

While the examples above show the basic usage of the `Substring()` method, there are a few other things to keep in mind when extracting substrings in C#:

- The starting index for `Substring()` is zero-based, meaning the first character has an index of 0.
- If you don't specify a length in the `Substring()` method, it will extract all the characters from the starting index until the end of the string.
- If the starting index or length is out of range, an `ArgumentOutOfRangeException` will be thrown.
- Substring extraction in C# is culture-sensitive, meaning it takes into account the culture of the system the code is running on. This can affect the behavior of methods such as `IndexOf()`.

For more information on the `Substring()` method and other string manipulation methods in C#, check out the official documentation [here](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring).

## See Also

- [String Manipulation in C#](https://www.c-sharpcorner.com/blogs/string-manipulation-in-c-sharp)
- [Using the Substring Method in C#](https://www.c-sharpcorner.com/article/using-the-substring-method-in-c-sharp/)

Happy coding!