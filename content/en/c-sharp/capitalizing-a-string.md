---
title:    "C# recipe: Capitalizing a string"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to all uppercase or lowercase letters? Maybe you're creating a program that has specific naming conventions, or maybe you just want to manipulate user input. Whatever the reason may be, capitalizing a string is a common task in programming. In this blog post, we will explore how to capitalize a string in C#.

## How To

In C#, there are several ways to capitalize a string. Let's take a look at a few examples:

```C#
// Using the ToUpper() method to capitalize all letters in a string

string name = "john doe";
string capitalized = name.ToUpper(); // JOHN DOE

```

```C#
// Using the ToLower() method to make all letters lowercase in a string

string name = "JOHN DOE";
string lowercase = name.ToLower(); // john doe
```

```C#
// Using the CultureInfo class to capitalize the first letter in a string

string name = "jane doe";
CultureInfo culture = new CultureInfo("en-US");
TextInfo textInfo = culture.TextInfo;
string capitalized = textInfo.ToTitleCase(name); // Jane Doe
```

As you can see, there are different methods and techniques to capitalize a string in C#. Depending on the specific requirements of your program, you can choose which approach works best for you. It is also important to note that these methods do not change the original string, but rather return a new string with the desired capitalization.

## Deep Dive

Under the hood, the ToUpper() and ToLower() methods use the ASCII table to convert letters to uppercase or lowercase. Each letter has a corresponding numeric value, and by adding or subtracting a certain number, we can convert it to uppercase or lowercase respectively. The CultureInfo class uses a more sophisticated approach by taking into account language-specific rules for capitalization.

It is also worth mentioning that the C# language provides a method called "capitalize" for strings, but this method is only available in certain versions of the .NET framework. Additionally, there are third-party libraries that offer more advanced functionality for capitalizing strings, such as handling special characters or multiple languages.

In conclusion, capitalizing a string may seem like a simple task, but there are various methods and techniques that can be used. Knowing the differences between these approaches can help you choose the best option for your specific programming needs.

## See Also

- [Microsoft Documentation on ToUpper() method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [C# CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [Third-party library for capitalizing strings in C#](https://www.nuget.org/packages?q=capitalize%20string&sortOrder=relevance&page=0)