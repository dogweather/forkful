---
title:                "Finding the length of a string"
html_title:           "PowerShell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is a common task in programming, especially when dealing with user input or manipulating text data. It allows programmers to understand the size and structure of a string, which can be useful in various scenarios such as data validation and formatting.

## How to:

To find the length of a string in PowerShell, we can use the built-in ```Length``` property. This property returns the number of characters in a string, including spaces and special characters. Here is an example of how to use it:

```
$str = "Hello world!"

$str.Length
```

The output of this code would be ```12```, as there are 12 characters in the string including the space between "Hello" and "world".

We can also use the ```Measure-Object``` cmdlet, which allows us to get the length of multiple strings at once. Here is an example:

```
$str1 = "Hello"
$str2 = "world!"
$str3 = "How are you?"

$str1, $str2, $str3 | Measure-Object -Property Length -Sum
```

The output of this code would be:

```
Count    : 3
Average  :
Sum      : 17
Maximum  :
Minimum  :
Property : Length
```

This tells us that the sum of all three strings' lengths is 17 characters.

## Deep Dive:

There are other ways to find the length of a string in PowerShell, such as using the [System.String] type accelerator. This allows us to use the ```.Length``` method on a string, like this:

```
$str = "Hello world!"

([System.String]$str).Length
```

The output would be the same as using the built-in ```Length``` property. However, this method can be useful when working with more complex string manipulations.

Another alternative is using regular expressions, which can provide more advanced ways to find the length of a string. However, it requires a deeper understanding of regular expressions and may not be necessary for simple tasks.

When it comes to the implementation details, PowerShell uses the .NET framework's String class to represent strings. This class has a ```Length``` property that is used to get the size of a string.

## See Also:

- [Microsoft Docs - About Strings in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1)
- [TechNet - PowerShell String Manipulation](https://social.technet.microsoft.com/wiki/contents/articles/3787.powershell-string-manipulation-getting-the-length-of-a-string.aspx)