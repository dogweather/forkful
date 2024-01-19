---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text means finding a certain text in a string and swapping it out for something new. It's a key part of programming as it streamlines data modification, allowing things like formatting, cleaning data, and changing content on-the-fly.

## How to:

Here's a simple C# example of searching and replacing text. We're using the `String.Replace()` method:

```C#
string OGString = "Hello, World!";
string NewString = OGString.Replace("Hello", "Goodbye");
Console.WriteLine(NewString);
```

When you run this code, it outputs:

```C#
Goodbye, World!
```

We just replaced "Hello" with "Goodbye."

## Deep Dive

The `String.Replace()` method has been around since the first version of C#. Before it or in its absence, you'd have to find the position of your search text, remove it, and then insert the replacement—a much slower process.

Alternatives exist. If you needed to replace text based on a pattern instead of exact matches, you'd use `Regex.Replace()`. This method employs regular expressions to match search patterns, providing a powerful tool for complex replacements:

```C#
string OGString = "The date is 2022-08-20.";
string NewString = Regex.Replace(OGString, @"\d{4}-\d{2}-\d{2}", "YYYY-MM-DD");
Console.WriteLine(NewString);
```

Here, we're replacing generic date format with the text "YYYY-MM-DD." Running this, you'd see:

```C#
The date is YYYY-MM-DD.
```

Under the hood, these methods turn the provided string into a character array, iterate through each character, identify matches, and swap out the desired text where applicable.

## See Also

Need more? Start with basics of `String.Replace()` in Microsoft Docs: [link](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)

A deep dive into C# Regex Class: [link](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)

For a complete C# string guide, check here: [link](https://www.tutorialsteacher.com/csharp/csharp-string)