---
title:                "Capitalizing a string"
html_title:           "C# recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string in C# refers to converting all the characters in a string to uppercase letters. This is useful for making data more visually consistent and for following certain programming conventions. It also allows for easier string manipulation and comparison when all letters are in the same case.

## How to:
To capitalize a string in C#, we can use the `ToUpper()` method from the `String` class. This method takes in a string and returns a new string with all characters converted to uppercase. Here is an example of how to use it:

```C#
string word = "hello";
string capitalizedWord = word.ToUpper();
Console.WriteLine(capitalizedWord);

// Output: HELLO
```

We can also use the `ToUpper()` method along with the `ToLower()` method to toggle between capitalization and lowercase. Here is an example of how to do this:

```C#
string sentence = "HeLlO WoRlD";
string toggledSentence = sentence.ToUpper().ToLower();
Console.WriteLine(toggledSentence);

// Output: hello world
```

## Deep Dive:
Capitalization has been around for centuries, with the concept originating from the Latin alphabet. In programming, capitalization is often used to follow certain conventions, such as using uppercase letters for constants and lowercase letters for variables. However, not all programming languages are case-sensitive, meaning that the casing of letters does not affect the outcome of the code.

In addition to using the `ToUpper()` method, there are other ways to capitalize a string in C#. One alternative is to use the `CultureInfo` class, which allows for more advanced string manipulation and casing based on specific cultures.

When it comes to the implementation of the `ToUpper()` method, it uses the `TextInfo.ToUpper()` method internally, which in turn uses a combination of Unicode data and culture-specific rules to convert the string to uppercase. This allows for consistent and accurate results no matter what language the string is in.

## See Also:
To learn more about capitalizing strings in C#, check out the official documentation: 
https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper

Additional resources on cultural differences in casing and string manipulation:
https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/