---
title:                "Capitalizing a string"
html_title:           "PowerShell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string in programming refers to converting all letters in a string to uppercase or lowercase, depending on the desired output. Programmers often do this to standardize the formatting of strings and make them easier to compare or manipulate.

## How to:
To capitalize a string in PowerShell, use the built-in `ToUpper()` or `ToLower()` methods. Here's an example:

```PowerShell
# Convert a string to uppercase
"My name is John" | ToUpper

#Output:
MY NAME IS JOHN

# Convert a string to lowercase
"HELLO WORLD!" | ToLower

#Output:
hello world!
```

## Deep Dive:
In the earlier days of computing, text was often written in uppercase letters due to technical limitations. However, modern programming languages allow for manipulating text easily, including converting it to uppercase or lowercase. In addition to the `ToUpper()` and `ToLower()` methods, PowerShell also has the `ToTitleCase()` method, which capitalizes the first letter of each word in a string.

There are alternate ways to capitalize strings, such as using loops and conditional statements. However, using built-in methods is the most efficient and preferred method for capitalizing strings.

## See Also:
For more information on using strings in PowerShell, refer to the official [Microsoft Docs](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/09-strings).
You can also explore the [PowerShell String Formatting](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/12-string-formatting) guide to learn different ways of manipulating strings in PowerShell.