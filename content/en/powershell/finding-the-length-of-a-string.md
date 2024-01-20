---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Finding the Length of a String in PowerShell

## What & Why?
Finding the length of a string involves determining the number of characters in a particular string data type. Programmers often need this to control logic flow, check data validity, or measure inputs for reasons such as performance optimization or data security.

## How to:
Get the length of a string in PowerShell using the `Length` property:

```PowerShell
$str = "Hello, world"
$str.Length
```

This would give the output `12`, which is the number of characters in `Hello, world`.

## Deep Dive
Determining the length of a string can be traced back to C, where you'd be looping through the char array until reaching the null terminator. Thankfully, PowerShell, inheriting from .NET, makes this much simpler through the `Length` property.

There are alternative ways, like converting the string into a character array and using the `Count` property:

```PowerShell
$str = "Hello, world"
$str.ToCharArray().Count
```
This yields the same result but is a bit indirect and may be slower for large strings due to the array conversion.

One implementation detail to note about `Length` is that it doesn't count bytes, but rather characters. So, multi-byte characters (like emojis) are still counted as one character:

```PowerShell
$str = "Hello, 🌍"
$str.Length
```
This would output `8`.

Also, `Length` will return `0` for null or empty strings:

```PowerShell
$str = ""
$str.Length
```

## See Also
- Learn more about strings in [Strings in .NET and PowerShell](https://devblogs.microsoft.com/scripting/understanding-powershell-and-basic-string-formatting/).
- Understand more about the Length property in [.NET's official Microsoft documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0).
- For string manipulation techniques in PowerShell, see [An Introduction to String Functions in PowerShell](https://www.computerperformance.co.uk/powershell/powershell-string-functions/).