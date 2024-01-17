---
title:                "Searching and replacing text"
html_title:           "C# recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming that involves finding specific words or phrases in a piece of code and replacing them with something else. Programmers do this to make their code more readable, to fix errors, or to make changes to a large amount of code at once.

## How to:

To search and replace text in C#, you can use the built-in `string.Replace()` method. This method takes in two parameters: the text to search for and the text to replace it with. Here's an example:

```C#
string oldString = "Hello World!";
string newString = oldString.Replace("World", "Universe");
Console.WriteLine(newString);
```

The output of this code would be `Hello Universe!`. We first created a string containing the phrase "Hello World!" and then used the `Replace()` method to replace the word "World" with "Universe". The result was assigned to a new variable called `newString` and then printed to the console.

You can also perform a case-sensitive search and replace by using the overload of the `Replace()` method that takes in a `StringComparison` parameter. Here's an example:

```C#
string oldString = "Hello World!";
string newString = oldString.Replace("world", "Universe", StringComparison.CurrentCulture);
```

In this case, the word "World" would not be replaced because the capitalization does not match with the searched word.

## Deep Dive:

Before the advent of integrated development environments (IDEs), programmers had to manually search through their code to find and replace text. This was a tedious and time-consuming process. With the use of IDEs and the `string.Replace()` method, this task has become much easier and quicker. 

Aside from the `Replace()` method, there are also other ways to search and replace text in C#. One option is to use regular expressions, which allows for more complex pattern matching. Another option is to use third-party libraries or packages that offer advanced search and replace functionality.

Under the hood, the `Replace()` method uses the `StringBuilder` class to perform the replacement. This class provides efficient string manipulation methods, making the `Replace()` method faster than traditional string concatenation.

## See Also:

- [String.Replace Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [StringBuilder Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [Regular Expressions in C# (C# Corner)](https://www.c-sharpcorner.com/UploadFile/972c03/regular-expression-with-C-Sharp-console-application/)