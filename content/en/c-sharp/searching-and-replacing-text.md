---
title:                "C# recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why 

In programming, it is not uncommon to encounter situations where text needs to be replaced within a file. This could be for formatting purposes, data manipulation, or any other specific task. In such cases, it is important to have an efficient method of searching and replacing text to save time and effort. In this blog post, we will explore the different ways to do so using C#.

## How To

There are a few ways to search and replace text in a file using C#. The most common method is using the `Regex` class to perform regular expression searches and replacements. The `Regex` class provides various methods such as `Match()`, `Replace()`, and `ReplaceMany()` that can be used to perform different types of search and replace operations. Below is a basic example using a regular expression to search and replace text in a file:

```
C#
string fileContent = File.ReadAllText("example.txt");
string pattern = @"([a-z]+) (\d+)";
string replacement = "$2 $1";

string updatedContent = Regex.Replace(fileContent, pattern, replacement);

Console.WriteLine(updatedContent);
```

In the example above, the `Regex` class is used to find patterns matching lowercase alphabets followed by numbers, and then replace them with the capture groups in reverse order. The output should include the replaced text in the desired format.

Another commonly used method for searching and replacing text is using the `StringBuilder` class. This class provides efficient ways to manipulate strings and also has a `Replace()` method that allows for text replacement. Below is an example of using `StringBuilder` to search and replace text:

```
C#
StringBuilder sb = new StringBuilder();
sb.AppendLine("This is some text");
sb.AppendLine("This is some more text");
sb.AppendLine("Even more text");

sb.Replace("text", "content");

Console.WriteLine(sb.ToString());
```

In the above example, the `Replace()` method is used to change all instances of `"text"` to `"content"` in the `StringBuilder` object. The output should include the updated content with the replaced text.

## Deep Dive

Regular expressions can be a powerful tool for searching and replacing text, but they can also be complex and difficult to read. One useful tip when working with regular expressions is to use online tools such as Regex101 or RegExr to test and debug your patterns before implementing them in your C# code.

Another thing to keep in mind is that regular expressions can also be used for advanced search and replace operations, such as capturing specific groups of text and using them in the replacement text. This can be done by using capture groups and referencing them using back-references, denoted by `"$N"` where N represents the number of the capture group.

## See Also

- [Microsoft C# Regex Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Microsoft C# StringBuilder Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netcore-3.1)
- [Regex101](https://regex101.com/)
- [RegExr](https://regexr.com/)