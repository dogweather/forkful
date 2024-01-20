---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a common task in many programming and data processing scenarios. It involves finding a particular string (sequence of characters) within a larger string or set of data and replacing it with a different string. This is a staple task when dealing with files, user inputs, or data cleaning.

## How to:
PowerShell offers an easy and efficient way to perform search and replace text operations. The most common way is to use the `-replace` operator. Here is an example of how to replace 'Hello' with 'Hi' in a string:

```PowerShell
$string = "Hello, World"
$string -replace 'Hello', 'Hi'
```
The output would be: 
```PowerShell
"Hi, World"
```
Multiple replacements in a string can be done by chaining `-replace` operations:
```PowerShell
$string = "Hello, Hello World!"
$string -replace 'Hello', 'Hi' -replace 'World', 'Everyone'
```
Output:
```PowerShell
"Hi, Hi Everyone!"
```

## Deep Dive

Historically search and replace operations have been performed using Regular Expressions, a sequence of characters that forms a search pattern. PowerShell's `-replace` operator supports using regular expressions for complex search patterns. An example with regular expression to replace any number with 'Num' will look like this:

```PowerShell
$string = "Order 5 apples and 7 oranges"
$string -replace '\d+', 'Num'
```
Output:
```PowerShell
"Order Num apples and Num oranges"
```

Alternatives exist as well. The 'System.String' `.Replace()` method in PowerShell is another option that can be handy, especially when you don't need the complexity of a regular expression. However, note that `.Replace()` is case-sensitive, unlike `-replace`.

```PowerShell
$string = "Hello, World"
$string.Replace('Hello', 'Hi')
```
Output:
```PowerShell
"Hi, World"
```

## See Also
For more insights and details on search and replace text in PowerShell, you can refer to the following resources:

1. [Microsoft - About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
2. [Microsoft - Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)