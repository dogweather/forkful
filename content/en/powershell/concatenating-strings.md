---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings means joining two or more strings together to form a single string. Programmers do this to manage and manipulate data in a flexible way, build dynamic statements, and output readable information to users.

## How To

PowerShell provides numerous ways to concatenate strings. 

### Using "+" Operator

```PowerShell
$firstName = "John"
$lastName = "Doe"
$message = "Hello, " + $firstName + " " + $lastName
Write-Output $message
```
Output:
```PowerShell
Hello, John Doe
```

### Using "-f" Operator

```PowerShell
$firstName = "John"
$lastName = "Doe"
$message = "Hello, {0} {1}" -f $firstName, $lastName
Write-Output $message
```
Output:
```PowerShell
Hello, John Doe
```

### Using string concatenation with $()

```PowerShell
$firstName = "John"
$lastName = "Doe"
$message = "Hello, $($firstName) $($lastName)"
Write-Output $message
```
Output:
```PowerShell
Hello, John Doe
```

## Deep Dive

The concatenation of strings in PowerShell has its roots in other older programming languages like C and C++. Over time, the "+", "-f" operators and the $() shorthand were introduced to make this process cake walk.

There are alternatives to string concatenation. For instance, you can use join, format and interpolate strings. Join is useful when dealing with arrays of strings, format and interpolate are handy when inserting variables into strings.

As for implementation details, in PowerShell "+", "-f" and "$()" perform string concatenation in their unique ways. The "+" operator simply combines strings, "-f" formats strings similar to sprintf in C, while "$()" allows you to put variables right inside a string.

## See Also

Here are some of the useful links for more information.

1. String concatenation in PowerShell: https://ss64.com/ps/syntax-concat.html
2. Alternative approaches: https://devblogs.microsoft.com/scripting/combining-strings-in-powershell/
3. Deep Dive into Concatenating Strings in PowerShell: https://adamtheautomator.com/powershell-string-concatenation/