---
title:                "Converting a string to lower case"
html_title:           "PowerShell recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case means changing all the letters in a given string to their lowercase equivalent. Programmers do this to standardize the input and make it easier to compare or manipulate the string later on. This can also help with data processing and sorting in database systems. 

## How to:
To convert a string to lower case in PowerShell, we can use the `ToLower()` method. Here is an example that converts the string "THIS IS A TEST" to lower case and stores it in a new variable called `$lowercase`.

```PowerShell
$uppercase = "THIS IS A TEST"
$lowercase = $uppercase.ToLower()
Write-Output $lowercase
```

The output would be: `this is a test`

We can also convert a string to lower case directly without creating a new variable. Here is an example:

```PowerShell
Write-Output "HELLO WORLD".ToLower()
```

The output would be: `hello world`

## Deep Dive:
Converting a string to lower case has been a common practice in programming for a long time. In older programming languages, such as C, methods like `tolower()` or `strlwr()` were used to achieve the same result. However, in PowerShell, the `ToLower()` method is the most efficient way to convert a string to lower case. 

An alternative to using the `ToLower()` method would be to use the built-in `Get-Culture` cmdlet to get the current system culture and use its `.TextInfo.ToLower()` method to convert the string. However, this method may not be as efficient as using the `ToLower()` method.

The `ToLower()` method works by changing the ASCII code of each character in the string to its lowercase equivalent. This ensures that all letters, regardless of their original case, are converted accurately. It also works with special characters and symbols in the string.

## See Also:
For more information about converting strings in PowerShell, refer to the official Microsoft documentation on [String.ToLower() Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8).