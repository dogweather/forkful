---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output is the process of outputting internal program state information to a console or log while a program runs. Programmers use it to understand program behavior and track down strange or unanticipated issues.

## How to:
To print debug information to the PowerShell console, accessorily known as "Standard Output," you will generally use the Write-Debug cmdlet. 

Here's an example:

```PowerShell
# turns on the debug preference
$DebugPreference = 'continue' 

# debug information
Write-Debug "This is some debug information"
```
This script will output:
```PowerShell
DEBUG: This is some debug information
```
Please remember that by default `$DebugPreference` is set to `'SilentlyContinue'` which means no debug messages will be output until you change the setting.

## Deep Dive
PowerShell's Write-Debug cmdlet is part of a larger suite of tools for outputting information to various channels. These channels, or "streams," include the error, warning, verbose, and information logs in addition to the debug stream. 

In the early days of programming, debugging was often accomplished via careful reading of code and trace routines. However, modern debugging in PowerShell and other languages offers much more sophisticated capabilities. 

You do have alternatives. For example, you might use Write-Host, but this directly writes to the host and not to the pipeline, thereby breaking the stream of information with other parts of the script. Similarly, Write-Output sends data down the pipeline and can potentially mess up the output of your command. Write-Debug, conversely, doesn't interfere with other channels and can be enabled or disabled easily.

The implementation of Write-Debug in PowerShell is part of Microsoft's wider initiative to make the language friendly for developers and system administrators who manage tasks with complex outputs. 

## See Also
1. [Write-Debug Documentation](https://docs.microsoft.com/powershell/module/Microsoft.PowerShell.Utility/Write-Debug)