---
title:                "Deleting characters matching a pattern"
html_title:           "PowerShell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a certain pattern is a common task in programming. It involves removing specific characters from a string based on a set of rules or conditions. This is often done to clean up data or manipulate strings in a specific way. Programmers may need to do this for a variety of reasons, such as formatting data for a database or making sure input values meet certain criteria.

## How to:

To delete characters matching a pattern in PowerShell, you can use the ```-replace``` operator along with a regular expression. Let's say we have a string that contains both numbers and letters, and we want to remove all the numbers. Here's how we can do it:

```powershell
$string = "W3lc0m3 t0 my H0m3"
$string -replace "\d", ""
```

The above code will output: "Welcome to my Home". In the first line, we assign the string to a variable called ```$string```. Then, in the second line, we use the ```-replace``` operator to replace all digits (represented by the ```\d``` regular expression) with an empty string. This essentially deletes all the numbers from the string.

You can also use the ```-replace``` operator with more complex regular expressions to remove multiple characters or patterns from a string. For example, if we want to remove all vowels from a string, we can use this code:

```powershell
$string = "Hello world!"
$string -replace "[aeiou]", ""
```

The output will be: "Hll wrld!". In this case, the regular expression ```[aeiou]``` matches any vowel in the string, and replaces it with an empty string.

## Deep Dive:

Deleting characters matching a pattern has been a common task in programming for a long time. It is often used in data cleaning and manipulation, as well as in building search and validation functions. In PowerShell, the ```-replace``` operator is the most common method for deleting characters matching a pattern. However, other methods such as regular expressions, split and join operations, and string manipulation functions can also be used.

Regular expressions are an incredibly powerful tool for pattern matching and manipulation in many programming languages. They allow you to specify complex patterns and conditions for matching, making them ideal for more advanced deletion tasks. In PowerShell, regular expressions are represented by the ```-match``` and ```-notmatch``` operators.

Split and join operations allow you to split a string into an array of smaller strings, and then join them back together using a delimiter. This can be useful when you only want to remove certain parts of a string, or when you want to manipulate individual elements of a string separately.

Built-in string manipulation functions, such as ```Trim()``` and ```Substring()```, can also be used to delete specific characters or patterns from a string in PowerShell.

## See Also:

- Regular expressions in PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions
- Split and Join string operations: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/split-string
- Built-in string manipulation functions in PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/strings-and-formatted-output-about