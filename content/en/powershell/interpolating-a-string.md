---
title:                "Interpolating a string"
html_title:           "PowerShell recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in PowerShell refers to the process of embedding a variable or expression within a string value. This allows for more dynamic and readable code, as variables and values can be easily inserted into a string instead of concatenating multiple strings together. Programmers use string interpolation to simplify their code and make it more efficient.

## How to:
To interpolate a string in PowerShell, simply use the `$` symbol before the variable or expression within the string. Let's see an example of interpolating a variable:

```PowerShell
$name = "John"
Write-Host "Hello $name, welcome to my PowerShell tutorial!"
```

The output of this code would be:

```
Hello John, welcome to my PowerShell tutorial!
```

We can also interpolate expressions within a string. For example:

```PowerShell
$x = 2
Write-Host "The value of x squared is $($x * $x)"
```

The output of this code would be:

```
The value of x squared is 4
```

It's important to note that the expression must be enclosed in `$( )` when being interpolated within a string.

## Deep Dive:
String interpolation in PowerShell was first introduced in version 3.0, making it a relatively new feature compared to other programming languages. Before version 3.0, concatenation was the only way to combine strings and variables. With string interpolation, code becomes more concise and readable.

An alternative to string interpolation is using the `Format` method and placing placeholders within a string. However, this requires an additional step and is not as intuitive as string interpolation.

Behind the scenes, the PowerShell engine translates interpolated strings into `String.Format` method calls. This means that interpolated strings are converted to strings with placeholders before being executed, which can impact performance.

## See Also:
- [PowerShell String Interpolation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/about/about_strings?view=powershell-7.1#string-interpolation) in Microsoft Docs.
- [PowerShell Concatenation vs String Interpolation](https://stackoverflow.com/questions/50526718/string-interpolation-powershell-vs-concatenation/50527245) on Stack Overflow.
- [The `String.Format` Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0) in .NET documentation.