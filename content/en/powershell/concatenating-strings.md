---
title:                "Concatenating strings"
html_title:           "PowerShell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the act of combining multiple strings into one single string. Programmers use this technique to create dynamic and customizable output based on different variables and inputs. It allows for more flexibility in text manipulation and makes code more efficient by reducing the number of lines needed to achieve the same result.

## How to:

To concatenate strings in PowerShell, you can use the `+` operator or the `Join-Path` cmdlet.

```
# Using the + operator
$name = "John"
$age = 32

"Hello " + $name + ", you are " + $age + " years old."

# Output: Hello John, you are 32 years old.

# Using Join-Path
$folder = "C:\Documents"
$file = "report.txt"

Join-Path $folder $file

# Output: C:\Documents\report.txt
```

## Deep Dive:

Concatenation is a common task in programming and it dates back to the earliest programming languages. In PowerShell, the `+` operator is the simplest and most common way to concatenate strings, but it can also be used to combine other types of data such as numbers and arrays.

An alternative to using the `+` operator is the `Join-String` cmdlet, which offers more flexibility and control over the concatenation process. It allows you to specify a delimiter to add between each string, and even join multiple strings together at once.

In terms of implementation, concatenating strings in PowerShell is straightforward and does not involve any complex algorithms or processes. However, it is important to note that string concatenation can affect the performance of your code if done excessively. This is because each time you concatenate strings, a new string object is created in memory.

## See Also:

- [PowerShell Documentation on String Concatenation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)
- [PowerShell Help for the Join-Path Cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/join-path?view=powershell-7)
- [C# String Concatenation vs StringBuilder](https://stackoverflow.com/questions/3055024/string-concatenation-vs-stringbuilder/3055042#3055042) (Note: While this is not specific to PowerShell, it provides an interesting comparison of different string manipulation methods.)