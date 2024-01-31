---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:53:05.849971-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Debug output is like a breadcrumb trail in a dark forest – those print statements we use to follow our code's execution. Programmers print debug output to understand what's happening, especially when things go awry.

## How to: (Як це зробити:)

PowerShell's basic command for output is `Write-Host`, but for debug-specific output, we use `Write-Debug`. Remember, `Write-Debug` is silenced by default. To see the debug messages, you need to set `$DebugPreference` or use the `-Debug` parameter.

```PowerShell
# Basic output
Write-Host "This is a regular message."

# Debug output
Write-Debug "This is a debug message."

# To see the debug message without changing global preference
Write-Debug "Here's a debug message with -Debug parameter." -Debug

# To change the preference globally
$DebugPreference = 'Continue'
Write-Debug "Now you see all debug messages!"
```

Output for this would be:

```
This is a regular message.
DEBUG: This is a debug message.
DEBUG: Here's a debug message with -Debug parameter.
DEBUG: Now you see all debug messages!
```

## Deep Dive (Поглиблений аналіз):

Once upon a time, debugging was about squashing physical bugs that shorted circuits. Today we've got more sophisticated methods to track down errors. `Write-Debug` follow this tradition by allowing developers to peek inside their running scripts.

There are other ways to debug in PowerShell. Besides `Write-Debug`, you can leverage `Write-Verbose` for detailed info, `Write-Warning` for potential issues, and `Write-Error` for actual errors. Tools like the PowerShell debugger, breakpoints, and the Integrated Scripting Environment (ISE) are for more complex scenarios.

`Write-Debug` outputs to the debug stream, one of the six streams in PowerShell. This design lets you control where and how much information you spill out. It's all about managing noise – ensuring that you get the signals you need without the irrelevant chatter.

## See Also (Дивіться також):

- [About Preference Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Preference_Variables)
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
- [PowerShell Streams](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Redirection)
- [Effective Debugging with PowerShell ISE](https://docs.microsoft.com/en-us/powershell/scripting/windows-powershell/ise/how-to-debug-scripts-in-windows-powershell-ise)
