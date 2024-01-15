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

## Why

Capitalizing a string is a common task in many programming languages, including C#. It is often required when working with user inputs or data that needs to be formatted in a specific way. By capitalizing a string, you can ensure consistency and readability in your code and output.

## How To

To capitalize a string in C#, you can use the `ToUpper()` method in the `String` class. This method converts all lowercase characters in a string to uppercase. Here's an example:

```C#
// Input string
string name = "john";

// Capitalize the string using ToUpper() method
string capitalized = name.ToUpper();

Console.WriteLine(capitalized);
// Output: JOHN
```

You can also use the `CultureInfo` class to specify the language-specific casing rules for your string. For example:

```C#
// Input string
string sentence = "hello world";

// CultureInfo object for English (United States) culture
CultureInfo enUS = new CultureInfo("en-US");

// Capitalize the string using ToTitleCase() method
string capitalized = enUS.TextInfo.ToTitleCase(sentence);

Console.WriteLine(capitalized);
// Output: Hello World
```

It's worth noting that the `ToUpper()` and `ToTitleCase()` methods do not modify the original string, but instead return a new string with the capitalized characters. So if you want to save the capitalization in a variable, be sure to assign it to a new string.

## Deep Dive

While the `ToUpper()` and `ToTitleCase()` methods are convenient for simple string capitalization, they may not work as expected for more complex situations. For example, if your string contains special characters or punctuation, the capitalization may not be accurate.

In such cases, you can use the `TextInfo` class and its properties to handle specific situations. For instance, the `TextInfo.ANSICodePage` property can help with capitalization in strings that contain ANSI characters. Similarly, the `TextInfo.OEMCodePage` property is useful for strings with OEM (original equipment manufacturer) characters.

Moreover, the `ToUpper()` method uses the casing rules of the current culture by default. This can cause unexpected results if you have different cultures in your application. To avoid this, you can specify the culture using the `StringComparison` enum in the `ToUpper()` method, as shown here:

```C#
// Input string
string name = "müller";

// Capitalize the string using ToUpper() method and specify Turkish culture
string capitalized = name.ToUpper(StringComparison.CurrentCultureIgnoreCase);

Console.WriteLine(capitalized);
// Output: MÜLLER (if current culture is Turkish)
```

## See Also

- [C# String.ToUpper() Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [C# TextInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo)