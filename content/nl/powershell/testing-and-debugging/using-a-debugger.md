---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:43.713165-07:00
description: "Hoe te: In PowerShell kun je scripts debuggen met de ingebouwde PowerShell\
  \ Integrated Scripting Environment (ISE) of Visual Studio Code (VS Code) met de\u2026"
lastmod: '2024-03-13T22:44:51.034166-06:00'
model: gpt-4-0125-preview
summary: In PowerShell kun je scripts debuggen met de ingebouwde PowerShell Integrated
  Scripting Environment (ISE) of Visual Studio Code (VS Code) met de PowerShell-extensie.
title: Een debugger gebruiken
weight: 35
---

## Hoe te:
In PowerShell kun je scripts debuggen met de ingebouwde PowerShell Integrated Scripting Environment (ISE) of Visual Studio Code (VS Code) met de PowerShell-extensie. Hier is hoe je breakpoints in beide kunt gebruiken:

### PowerShell ISE:
```PowerShell
# Stel een breakpoint in op een specifieke regel
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Voer je script normaal uit
.\MyScript.ps1

# Wanneer het script het breakpoint bereikt, kun je variabelen inspecteren
$myVariable

# Vervolg de uitvoering
Continue
```

### Visual Studio Code:
```PowerShell
# Open je PowerShell script in VS Code.
# Klik links van het regelnummer om een breakpoint in te stellen.
# Start het debuggen door op F5 te drukken of op 'Start Debugging' te klikken.

# VS Code zal de uitvoering bij je breakpoint stoppen.
# Gebruik het debug paneel om variabelen te bekijken, de call stack te inspecteren en de stroom te beheren.
```

Debuggen in beide omgevingen stelt je in staat om in te stappen (F11), over te stappen (F10) en uit te stappen (Shift+F11) tijdens het debuggen.

## Diepgaand
Historisch gezien was debuggen in PowerShell een beetje omslachtig; het vereiste veel `Write-Host` regels om variabele staten uit te voeren of de klassieke trial-and-error methode. Met de komst van PowerShell ISE, en meer recentelijk, VS Code met zijn rijke debugfuncties, werd het debuggen in PowerShell bijna net zo intuïtief als in volwaardige programmeertalen.

Alternatieven voor de native debugtools van PowerShell omvatten tools van derden zoals PowerGUI of het gebruik van robuuste IDE's zoals Visual Studio met een PowerShell-plugin.

Wanneer je een debugger implementeert, overweeg dan de scriptscope, vooral wanneer je werkt met dot-sourced scripts of modules. Breakpoints kunnen op conditie gebaseerd zijn, op variabele verandering gebaseerd, of op regel gebaseerd, wat een precieze controle mogelijk maakt tijdens een debugsessie.

Bovendien, met de overgang naar PowerShell Core (cross-platform PowerShell), is debuggen grotendeels in handen gekomen van VS Code, dat een consistente ervaring biedt op verschillende platformen.

## Zie Ook
Voor meer over debuggen in PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
