---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracting Substrings in C# - The Why's and How's 

## What & Why?

Extracting substrings is about pulling specific portions of a string based on your defined starting index and length. Programmers do this to manipulate, analyze, or filter string data efficiently.

## How to:

Let's dive straight into some code.

```C#
string str = "Hello, World!";
string substring = str.Substring(0, 5);
Console.WriteLine(substring);
```

Output:

```C#
Hello
```

In this example, we start at index zero (the very beginning of the string) and take the first five characters to form our substring.

Naturally, you can start at any index. Let's try extracting "World":

```C#
string str = "Hello, World!";
string substring = str.Substring(7, 5);
Console.WriteLine(substring);
```

Output:

```C#
World
```

We started at the 7th index and took the next 5 characters.

## Deep Dive

Now, why do we need this? Prior to .NET Framework, programmers relied on string manipulation methods such as Split() or parsing character arrays. Substring extraction simplified this process, making coding efficient and readable.

There are alternatives depending on context. For instance, if you're dealing with the end of a string, there are .NET methods such as EndsWith(). For more pattern-based extraction, Regular expressions can be used.

Under the hood, Substring() works by creating a new string object, copying the specified characters of the old string, and then freeing the old string if it's not referenced elsewhere.

## See Also

For more on string manipulation in C#, check the official C# documentation [here.](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
For a deeper understanding of the Substring method in C#, take a look at [this article.](https://www.dotnetperls.com/substring)
For alternative methods to handle strings, check [this StackOverflow link.](https://stackoverflow.com/questions/1082532/how-to-truncate-a-string-in-c-sharp)