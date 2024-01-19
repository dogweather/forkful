---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convert Strings to Lowercase in PowerShell: A Quick, Handy Guide

## What & Why?

In computer programming, converting a string to lower case means transforming all the uppercase letters into lower case. Why do programmers need to do it? For consistent data processing and comparison, avoiding case sensitivity issues.

## How to:

In PowerShell, here's how simple it is to convert a string to lowercase.

``` PowerShell
$s = "This Is A Sample String"
$s.ToLower()
```

If you run the above script, it will provide this output:

``` PowerShell
this is a sample string
```

## Deep Dive

Historically, handling case sensitivity in programming languages has its place. For a user, 'Hello' and 'HELLO' look identical but not for a computer. Hence, programmers convert strings to a standard case (often lowercase) for efficient comparisons and data processing.

In PowerShell, the `ToLower()` method comes from the .NET framework's `String` class. It's not unique to PowerShell but is a universal .NET method for string manipulation. 

There's also an alternative, the `ToUpper()`. Just as `ToLower()`, it changes all characters to one case – the upper case. It's also useful in handling case sensitivity, depending on the requirement.

Implementation wise, you might wonder what happens when a string has no uppercase characters. Simple – PowerShell's `ToLower()` doesn't make any changes. It leaves the string as is, doesn't cause errors or exceptions.

## See Also

1. [Official documentation](https://docs.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-string-comparisons?view=powershell-7.1) about string comparisons in PowerShell, courtesy of Microsoft.
2. [Converting Strings to DateTime](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/convert-string?view=powershell-7.1) in PowerShell.
3. For more into the .NET framework's `String` class, check out [Microsoft’s Official Documentation](https://docs.microsoft.com/dotnet/api/system.string?view=net-5.0).