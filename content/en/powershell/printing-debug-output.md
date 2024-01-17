---
title:                "Printing debug output"
html_title:           "PowerShell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is a way for programmers to check the execution and behavior of their code during development and troubleshooting. It allows them to see the internal workings of their program and identify any errors or unexpected results.

## How to:

To print debug output in PowerShell, use the ```Write-Debug``` cmdlet and enclose the code you want to debug within a ```Debug {...}``` block. For example:

```
Write-Debug "Starting debug output"

Debug {
    # Code to be debugged goes here
    $x = 5
    $y = 10
    Write-Debug "x = $x"
    Write-Debug "y = $y"
    Write-Debug "x + y = $($x+$y)"
}

Write-Debug "Finished debug output"
```

Running this code with the ```-Debug``` flag will display the output within the Debug block, as well as any other debug messages you have specified with the ```Write-Debug``` cmdlet.

## Deep Dive

Before the advent of integrated development environments (IDEs), printing debug output was the primary method for programmers to test and troubleshoot their code. It allowed them to see the flow of their program and identify any errors or unexpected results.

While IDEs now offer advanced features for debugging, printing debug output is still a useful tool for debugging small sections of code or for situations where an IDE is not available.

An alternative to using ```Write-Debug``` is to use the ```Write-Host``` cmdlet. However, this should be used sparingly as it can clutter the output and make it harder to identify important information.

Under the hood, the ```Write-Debug``` cmdlet uses the ```Write-Verbose``` cmdlet, but only prints the output when the ```-Debug``` flag is specified. This allows for more granular control over what information is displayed during debugging.

## See Also

For more information on debugging in PowerShell, check out the official Microsoft documentation on [debugging scripts](https://docs.microsoft.com/en-us/powershell/scripting/debugging/).

You can also explore alternative methods for debugging in PowerShell such as using the [PowerShell debugger](https://docs.microsoft.com/en-us/powershell/scripting/debugging/using-the-debugger) or [remote debugging](https://docs.microsoft.com/en-us/powershell/azure/running-script-commands-remotely?view=azps-5.0.0).