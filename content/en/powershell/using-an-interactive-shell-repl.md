---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:47.927093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
The interactive shell, or Read-Eval-Print Loop (REPL), lets you type PowerShell commands and get immediate feedback. Programmers use it to test code snippets quickly, debug, or learn new commands without writing a full script.

## How to:
Launch PowerShell and you're in the REPL. Try the `Get-Date` Cmdlet:

```PowerShell
PS > Get-Date
```

You should see the current date and time output:

```PowerShell
Wednesday, March 31, 2023 12:34:56 PM
```

Now, chain commands. Let's sort processes by memory usage:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

This outputs the top 5 processes by working set size (memory usage).

## Deep Dive
PowerShell's REPL has its roots in the Unix shell and other dynamic language shells like Python's. It's a single-user, interactive command execution environment. Unlike a compiled language where you write whole applications and then compile, a REPL environment lets you write and run code one line at a time. PowerShell also supports script execution for larger tasks.

Alternatives for Windows include the Command Prompt or other language-specific REPLs like IPython. In the Unix/Linux world, shells like bash or zsh serve a similar function.

PowerShell's implementation uses a host application to run the shell. While PowerShell.exe in Windows is the most common, others like the Integrated Scripting Environment (ISE) or Visual Studio Code's integrated terminal can also serve as the host.

## See Also
- [About PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)