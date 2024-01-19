---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting New Projects with PowerShell

## What & Why?

Jumping into a new project simply means initializing and setting up your workspace accurately. As programmers, we do this for optimized workflow, efficient organization, and less fuss when coding gets tough. It's all about setting yourself up for success.

## How To:

No high-level linguistics here. Let's dive into how to create a new PowerShell project. 

Run PowerShell as an admin and then proceed to create a new folder that will hold your project. Let’s make a ‘TestProject’:

```PowerShell
New-Item -ItemType Directory -Path . -Name TestProject
```

Set your current location to the new project:

```PowerShell
Set-Location -Path .\TestProject
```

Let’s create a new script file:

```PowerShell
New-Item -ItemType File -Path . -Name Main.ps1
```

Verify your project’s structure:

```PowerShell
Get-ChildItem -Path .
```

Output:

```
    Directory: C:\Path\to\TestProject
    
Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
-a----         2/24/2023  10:10 AM              0 Main.ps1
```

This means your new project, 'TestProject', is on its feet.

## Deep Dive

Windows PowerShell, now PowerShell 7.0 (a cross-platform edition), was introduced back in 2006 for task automation and configuration. It uses cmdlet's (lightweight command-line programs) which make starting a new project intuitive and efficient.

As an alternative, creating a project structure manually via a text editor is feasible but potentially time-consuming and error-prone. 

The fun part of PowerShell is in scripts. Scripts in PowerShell adhere to the *.ps1* extension convention. This setup is PowerShell’s superpower for cleaner, drier code. Then, running your scripts is as easy as invoking *powershell.exe ./Main.ps1*.

Always remember, the better-structured your projects are, the easier managing the project becomes.

## See Also

- Microsoft Document on PowerShell: https://docs.microsoft.com/en-us/powershell
- GitHub Repo of PowerShell: https://github.com/PowerShell/PowerShell
- Beginners Guide to PowerShell: https://www.computerworld.com/article/3237230/what-is-powershell-powershell-tutorial.html