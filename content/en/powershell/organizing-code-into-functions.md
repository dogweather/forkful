---
title:                "Organizing code into functions"
aliases:
- en/powershell/organizing-code-into-functions.md
date:                  2024-01-25T02:59:34.955331-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions is about wrapping up chunks of code that perform specific tasks and giving them a name. It's done to make code reusable, readable, and maintainable. Instead of rewriting the same code, call a function. Want to troubleshoot or upgrade? Tweak the function without sifting through piles of script.

## How to:
Let's write a function to calculate the sum of two numbers. Simple, but it illustrates the point.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Call the function with 5 and 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "The sum is $sum"
```

Sample output:

```
The sum is 15
```

## Deep Dive
Functions in PowerShell, like in most languages, are old news. We’ve been compartmentalizing code since the days of Fortran. It's about 'not reinventing the wheel'. Alternatives? Sure, scripts or cmdlets. But they lack the tidiness and context-sensitivity of functions within scripts.

Implementation? Functions can be basic like our example or complex with scopes, pipeline input, and more. Take `Advanced Functions`. They mimic cmdlets with parameters that have attributes, like `[Parameter(Mandatory=$true)]`. That's a taste of PowerShell's flexibility.

## See Also
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
