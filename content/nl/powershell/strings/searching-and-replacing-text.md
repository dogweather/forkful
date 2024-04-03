---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:14.191039-07:00
description: "Zoeken en vervangen van tekst in bestanden: het is het verwisselen van\
  \ woorden of zinsdelen door anderen. Programmeurs gebruiken het om code bij te\u2026"
lastmod: '2024-03-13T22:44:51.011449-06:00'
model: gpt-4-0125-preview
summary: 'Zoeken en vervangen van tekst in bestanden: het is het verwisselen van woorden
  of zinsdelen door anderen.'
title: Tekst zoeken en vervangen
weight: 10
---

## Wat & Waarom?
Zoeken en vervangen van tekst in bestanden: het is het verwisselen van woorden of zinsdelen door anderen. Programmeurs gebruiken het om code bij te werken, fouten te herstellen of gegevens snel over meerdere bestanden aan te passen zonder handmatig elk bestand door te hoeven nemen.

## Hoe:
PowerShell maakt zoeken en vervangen vrij eenvoudig. Bekijk `-replace` voor strings en `Get-Content` met `Set-Content` voor bestanden.

### Tekst in een string vervangen:
```PowerShell
$text = "I love PowerShell"
$updatedText = $text -replace "love", "adore"
$updatedText
```
Voorbeelduitvoer:
```
I adore PowerShell
```

### Tekst in een bestand vervangen:
```PowerShell
$file = "example.txt"
$content = Get-Content $file
$content | ForEach-Object { $_ -replace "oldWord", "newWord" } | Set-Content $file
```
Hier geen uitvoer, maar `example.txt` heeft nu elk "oudWoord" vervangen door "nieuwWoord".

## Diepere duik
Sinds het begin van tekstbewerking is zoeken en vervangen een hoeksteen geweest. Denk eraan als de zoek-en-vervang functie in een tekstverwerker maar dan super opgeladen voor codenoden.

Vroeger gebruikten commandoregel-tovenaars gereedschappen zoals `sed` in Unix. PowerShell bracht deze functionaliteit naar zijn scripttaal. Waarom is het cool? Omdat het gebonden is aan objecten, niet alleen aan tekst. Dat betekent dat je niet alleen code en tekstbestanden kunt aanpassen, maar ook gegevensstructuren en meer.

Alternatieven? Zeker. Je hebt teksteditors en IDE's met hun eigen zoek-en-vervang, batchscripts, of zelfs programmeerbibliotheken ontworpen voor tekstmanipulatie.

Implementatiedetails? PowerShell doet aan regex. Dat betekent dat je dingen kunt vervangen op basis van patronen, niet alleen vaste woorden. Plus, met PowerShell-scripts kun je deze bewerkingen automatiseren over een enorm aantal bestanden, wat je een hoop tijd bespaart.

## Zie ook
- PowerShell `-replace` operator documentatie: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- Het gebruik van `Get-Content` en `Set-Content`: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
