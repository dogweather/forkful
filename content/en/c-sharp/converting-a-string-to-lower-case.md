---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lowercase in C# programming means transforming all uppercase letters in the text string to their lowercase counterparts. It's regularly done to make text comparisons case-insensitive and normalize user input.

## How to:

In C#, converting a string to lowercase is quite straightforward. You can use the `ToLower` function like so:

```C#
string myString = "HELLO, WORLD!";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString); // Output: "hello, world!"
```
The program eloquently converts all the characters in myString to lowercase and displays the modified string on the console.

## Deep Dive

Historically, converting strings to lowercase has been part of text processing since the dawn of computing. It adds flexibility and consistency to our applications, especially ones dealing with user-provided input.

There are multiple ways to convert a string to lowercase besides using `ToLower`. Notably, you can use the `ToLowerInvariant` method, which uses culture-independent casing rules. It's the safest choice for culture-agnostic string comparisons.

```C#
string myString = "HELLO, WORLD!";
string lowerCaseString = myString.ToLowerInvariant();
Console.WriteLine(lowerCaseString); // Output: "hello, world!"
```

In terms of implementation details, the `ToLower()` and `ToLowerInvariant()` methods internally use culture-related information from the `TextInfo` object of the `CultureInfo` class to perform case conversion.

Keep in mind, converting a string to lowercase is not an in-place transformation. A new string will be created and returned because strings in C# are immutable.

## See Also

For more info on related subjects, refer to these documents:

- C# String ToLower() Method: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- CultureInfo Class: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo
- Learn about methods in C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
- String immutability in C#: https://www.csharpstar.com/csharp-string-immutability