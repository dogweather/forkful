---
date: 2024-01-20 17:57:04.121592-07:00
description: "Hur g\xF6r man: K\xF6r skript s\xE5 h\xE4r i PowerShell."
lastmod: '2024-04-05T21:53:39.478724-06:00'
model: gpt-4-1106-preview
summary: "K\xF6r skript s\xE5 h\xE4r i PowerShell."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Hur gör man:
```PowerShell
# Spara detta som test-script.ps1

# Skriv ut alla argument
param($Argument1, $Argument2, $Argument3)
Write-Host "Argument 1: $Argument1"
Write-Host "Argument 2: $Argument2"
Write-Host "Argument 3: $Argument3"

# Använda $args-array för att fånga alla argument
Write-Host "`nAlla argument med `$args:"
$args | ForEach-Object { Write-Host "Argument: $_" }
```
Kör skript så här i PowerShell:
```PowerShell
.\test-script.ps1 -Argument1 'värde1' -Argument2 'värde2' -Argument3 'värde3'
```
Du får detta:
```
Argument 1: värde1
Argument 2: värde2
Argument 3: värde3

Alla argument med $args:
Argument: värde1
Argument: värde2
Argument: värde3
```

## Fördjupning:
Kommandoradsargument i PowerShell har lång tradition, med rötter i Unix shell-scripting. PowerShell utökar detta med stöd för namngivna argument (som ovan) och möjligheten att behandla argument som starkt typade parametrar.

Andra alternativ för att läsa argument inkluderar `$host.ui.Prompt()` för interaktiva förfrågningar eller att använda avancerade parametrar och parametrark block för att definiera beteende som obligatoriska argument eller att validera input.

Detaljer kring implementation:
- `param()` definierar parametrar i början av scriptet.
- `$args`-arrayen används när parametrar inte är namngivna eller för att fånga ytterligare argument.
- `Write-Host` använder vi för att skriva output direkt till konsolen.

För mer komplexa argumenthantering kan man skapa en `Param`-block med `CmdletBinding` för att aktivera avancerade funktioner som `Mandatory`, `HelpMessage`, och `Parameter Sets`.

## Se även:
- [about_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters)
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters)
- [Scripting with Windows PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-arrays?view=powershell-7.1)
