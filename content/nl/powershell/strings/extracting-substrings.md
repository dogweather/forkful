---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:59.100885-07:00
description: 'Hoe te: Hier is hoe je strings kunt snijden en hacken in PowerShell.'
lastmod: '2024-03-13T22:44:51.015485-06:00'
model: gpt-4-0125-preview
summary: Hier is hoe je strings kunt snijden en hacken in PowerShell.
title: Substrings extraheren
weight: 6
---

## Hoe te:
Hier is hoe je strings kunt snijden en hacken in PowerShell:

```PowerShell
# Gegeven een string
$text = "Verbeter je PowerShell vaardigheden!"

# Extraheren met substring methode
$startIndex = 10
$lengte = 9
$substring = $text.Substring($startIndex, $lengte)
Write-Host $substring  # Uitvoer: je PowerS

# Extraheren met behulp van de reikwijdte operator
$subrange = $text[10..18] -join ''
Write-Host $subrange  # Uitvoer: je PowerS

# Extraheren vanaf het begin tot een bepaalde positie
$eersteDeel = $text.Substring(0, $startIndex)
Write-Host $eersteDeel  # Uitvoer: Verbeter 

# Extraheren na een bepaald karakter
$gesplitsteString = $text.Split(" ")[2]
Write-Host $gesplitsteString  # Uitvoer: je
```

## Dieper Duiken
Vroeger had PowerShell slechts basis stringmethoden. Nu, is het een heel ander spel. De `.Substring()` methode is er al een tijdje en is vrij eenvoudig - geef het een startindex en een optionele lengte, en het zal uitsnijden wat je nodig hebt. Vanaf PowerShell 6, kun je ook de reikwijdte operator gebruiken, wat eenvoudiger kan zijn, vooral als je werkt met strings van variabele lengte.

Er zijn ook de `-split` operator en de `.Split()` methode, beide handig voor het opdelen van strings op basis van patronen of karakters. Heb je een specifiek stuk nodig? Gebruik deze hulpmiddelen.

Prestatiegewijs is er niet veel verschil voor kleine taken. Wanneer je werkt met enorme tekstbestanden of elke milliseconde loopt, dan wil je benchmarks. Anders gaat het meer om leesbaarheid en wat goed voelt voor je script.

Onthoud, PowerShell strings worden geïndexeerd vanaf nul - gebruikelijk in veel programmeertalen. Let op de lastige off-by-one fout.

## Zie Ook
Voor meer over stringmanipulatie in PowerShell:

- [About_Split](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7)
- [Over Vergelijkingsoperatoren](https://docs.microsoft.com/nl-nl/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7) waar de -split behandeld wordt
