---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:41.211022-07:00
description: 'Hoe te: Je kunt de `-replace` operator gebruiken om aanhalingstekens
  uit een tekenreeks te verwijderen. Zo doe je dat.'
lastmod: '2024-03-13T22:44:51.014510-06:00'
model: gpt-4-0125-preview
summary: Je kunt de `-replace` operator gebruiken om aanhalingstekens uit een tekenreeks
  te verwijderen.
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe te:
Je kunt de `-replace` operator gebruiken om aanhalingstekens uit een tekenreeks te verwijderen. Zo doe je dat:

```PowerShell
# Enkele aanhalingstekens vervangen
$stringWithSingleQuotes = "'Hallo, Wereld!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Uitvoer: Hallo, Wereld!

# Dubbele aanhalingstekens vervangen
$stringWithDoubleQuotes = '"Hallo, Wereld!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Uitvoer: Hallo, Wereld!
```

Voor beide typen:

```PowerShell
$stringWithQuotes = '"Hallo daar," zei ze.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Let op het gebruik van regex-tekenklasse
Write-Output $cleanString  # Uitvoer: Hallo daar, zei ze.
```

Voorbeelduitvoer van de console ziet er ongeveer zo uit:

```
Hallo, Wereld!
Hallo, Wereld!
Hallo daar, zei ze.
```

## Diepere Duik
In de dagen voordat PowerShell een schittering in Microsoft's oog was, was het verwerken van tekst in Windows vaak het domein van batchscripts die beperkte mogelijkheden hadden. De introductie van PowerShell bracht krachtige functies voor tekenreeksmanipulatie met zich mee die scripting veel robuuster maakten.

Alternatieven voor `-replace` bestaan, zoals het gebruik van de `.Trim()` methode om aanhalingstekens alleen aan het begin en einde van een tekenreeks te verwijderen, maar deze bieden niet dezelfde controle of ondersteuning voor regex.

```PowerShell
# .Trim() gebruiken voor aanhalingstekens aan het begin en einde
$stringWithQuotes = '"Hallo, Wereld!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Uitvoer: Hallo, Wereld!
```

Let op, `-replace` gebruikt achter de schermen regex, dus als je ermee werkt, houd er rekening mee dat speciale tekens moeten worden ontsnapt als je ze target. Als je meer gedetailleerde controle over het verwijderen van aanhalingstekens nodig hebt, is het dieper induiken in regex met `-replace` de weg te gaan, waardoor je enorme flexibiliteit krijgt.

## Zie Ook
- Meer over regex in PowerShell, controleer de officiële documentatie: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Ontdek andere tekenreeksmethoden: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
