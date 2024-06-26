---
date: 2024-01-26 03:50:46.336525-07:00
description: "Wie geht das: In PowerShell k\xF6nnen Sie Skripte mithilfe der eingebauten\
  \ PowerShell Integrated Scripting Environment (ISE) oder Visual Studio Code (VS\u2026"
lastmod: '2024-03-13T22:44:54.108538-06:00'
model: gpt-4-0125-preview
summary: "In PowerShell k\xF6nnen Sie Skripte mithilfe der eingebauten PowerShell\
  \ Integrated Scripting Environment (ISE) oder Visual Studio Code (VS Code) mit der\
  \ PowerShell-Erweiterung debuggen."
title: Einsatz eines Debuggers
weight: 35
---

## Wie geht das:
In PowerShell können Sie Skripte mithilfe der eingebauten PowerShell Integrated Scripting Environment (ISE) oder Visual Studio Code (VS Code) mit der PowerShell-Erweiterung debuggen. So verwenden Sie Haltepunkte in beiden:

### PowerShell ISE:
```PowerShell
# Setzen eines Haltepunktes auf eine bestimmte Zeile
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Führen Sie Ihr Skript normal aus
.\MyScript.ps1

# Wenn das Skript auf den Haltepunkt trifft, können Sie Variablen untersuchen
$meineVariable

# Fortsetzen der Ausführung
Continue
```

### Visual Studio Code:
```PowerShell
# Öffnen Sie Ihr PowerShell-Skript in VS Code.
# Klicken Sie links von der Zeilennummer, um einen Haltepunkt zu setzen.
# Starten Sie das Debugging, indem Sie F5 drücken oder auf 'Start Debugging' klicken.

# VS Code stoppt die Ausführung am Haltepunkt.
# Verwenden Sie das Debug-Panel, um Variablen zu beobachten, den Aufrufstapel zu untersuchen und den Ablauf zu steuern.
```

Das Debuggen in beiden Umgebungen ermöglicht es Ihnen, während des Debuggings einzutreten (F11), darüber hinaus zu gehen (F10) und herauszugehen (Umschalttaste+F11).

## Tiefergehend
Historisch betrachtet war das Debugging in PowerShell etwas umständlich; es erforderte viele `Write-Host`-Zeilen, um Variablenzustände auszugeben oder die klassische Methode von Versuch und Irrtum. Mit der Einführung der PowerShell ISE und neuerdings von VS Code mit seinen umfangreichen Debugging-Funktionen wurde das Debugging in PowerShell fast so intuitiv wie in vollwertigen Programmiersprachen.

Alternativen zu den nativen Debugging-Tools von PowerShell umfassen Drittanbieter-Tools wie PowerGUI oder die Verwendung robuster IDEs wie Visual Studio mit einem PowerShell-Plugin.

Bei der Implementierung eines Debuggers sollte der Skriptumfang berücksichtigt werden, insbesondere beim Arbeiten mit dot-source-Skripten oder Modulen. Haltepunkte können bedingungsabhängig, variablenänderungsbasiert oder zeilenbasiert sein, was während einer Debugging-Sitzung eine präzise Kontrolle ermöglicht.

Darüber hinaus ist mit dem Übergang zu PowerShell Core (plattformübergreifendes PowerShell) das Debugging weitgehend in die Hände von VS Code übergegangen, das eine konsistente Erfahrung auf verschiedenen Plattformen bietet.

## Siehe auch
Für mehr zum Thema Debugging in PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
