---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:12.895532-07:00
description: "Hoe PowerShell leest command line argumenten met de `$args` array of\
  \ parameters. `$args` is snel voor eenmalige scripts; parameters zijn beter voor\u2026"
lastmod: '2024-04-05T22:38:49.907078-06:00'
model: gpt-4-0125-preview
summary: Hoe PowerShell leest command line argumenten met de `$args` array of parameters.
  `$args` is snel voor eenmalige scripts; parameters zijn beter voor robuuste tools.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe
PowerShell leest command line argumenten met de `$args` array of parameters. `$args` is snel voor eenmalige scripts; parameters zijn beter voor robuuste tools.

### Gebruikmakend van `$args`
```PowerShell
# myscript.ps1
Write-Host "Je hebt de volgende argumenten ingevoerd:"
$args
```
Uitvoeren met `.\myscript.ps1 Hallo PowerShell`, geeft als output:
```
Je hebt de volgende argumenten ingevoerd:
Hallo PowerShell
```

### Gebruikmakend van Parameters
```PowerShell
# myscriptparam.ps1
param (
    [string]$Name,
    [int]$Age
)
Write-Host "Hallo, $Name! Je bent $Age jaar oud."
```
Uitvoeren met `.\myscriptparam.ps1 -Name Sarah -Age 32`, geeft als output:
```
Hallo, Sarah! Je bent 32 jaar oud.
```

## Diepgaand
De moderne benadering van PowerShell voor command line argumenten is vergelijkbaar met een nalatenschap van zijn voorgangers zoals cmd en Bash. Echter, het versterkt de flexibiliteit en precisie.

### Historische Context
Jaren terug, benaderden batchbestanden en shellscripts argumenten met genummerde variabelen (zoals `%1`, `%2`). PowerShell verfijnde dit met `$args` en benoemde parameters voor meer duidelijkheid en controle.

### Alternatieven
Er zijn alternatieven, zoals het parseren van ruwe invoer met `Read-Host` of het accepteren van ingevoerde gegevens via een pijpleiding. Echter, `$args` en parameters zijn naadlozer voor geautomatiseerde taken en scripts.

### Implementatiedetails
`$args` is een eenvoudige array, goed voor willekeurige invoer. Parameters, met hun attributen en typen, kunnen invoer valideren en zelfs de gebruiker om informatie vragen, waardoor scripts zelfdocumenterend worden en minder vatbaar voor fouten.

## Zie ook
- [Over Parameters](https://docs.microsoft.com/nl-nl/powershell/scripting/developer/cmdlet/cmdlet-parameter-sets?view=powershell-7)
- [Automatische Variabelen in PowerShell](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7&viewFallbackFrom=powershell-6)
