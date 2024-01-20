---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is a method to insert variables directly into a string. It’s used by programmers to simplify and streamline code for readability and efficiency.

## How to:

In PowerShell, you implement string interpolation through a method known as "variable expansion". Here are code examples:

```PowerShell
$name = "Alice"
Write-Host "Hello, $name!"
```

Once the code is run, the output will look like this:

```PowerShell
Hello, Alice!
```

You can also use it with complex expressions within a string. Here's how:

```PowerShell
$x = 5
$y = 10
Write-Host "The sum of $x and $y is $($x + $y)"
```

The output will be:

```PowerShell
The sum of 5 and 10 is 15
```

## Deep Dive

Historically, languages like C# have extensively utilized string interpolation. PowerShell, first released in 2006, adopted this method and refined it over time. 

An alternative to string interpolation is using the traditional string concatenation. This involves manually adding the variable into the string through '+' symbol. But this can make the code look cluttered when dealing with multiple variables:

```PowerShell
Write-Host "The sum of " + $x + " and " + $y + " is " + ($x + $y)
```

PowerShell's implementation of string interpolation with variable expansion directly translates the variable into a string. Unlike some languages, PowerShell uses "$()" to evaluate more complex expressions rather than just simple variables. 

## See Also:

For more in-depth understanding and applications of string interpolation in PowerShell, you may check these links:

- Microsoft Documentation on [About Quoting Rules](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- Stack Overflow Discussion: [How do I do string interpolation in PowerShell?](https://stackoverflow.com/questions/3438518/how-do-i-do-string-interpolation-in-powershell)