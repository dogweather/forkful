---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:28.443821-07:00
description: "Hoe: Laten we wat tekst opvrolijken. In PowerShell, gebruik `.ToTitleCase`\
  \ van `System.Globalization` voor kapitalisatie zoals bij titels, of eenvoudige\u2026"
lastmod: '2024-03-13T22:44:51.009372-06:00'
model: gpt-4-0125-preview
summary: Laten we wat tekst opvrolijken.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe:
Laten we wat tekst opvrolijken. In PowerShell, gebruik `.ToTitleCase` van `System.Globalization` voor kapitalisatie zoals bij titels, of eenvoudige methodes zoals `.ToUpper()` of `.ToLower()` om de hoofdlettergebruik te veranderen.

```PowerShell
# Laad de TextInfo klasse om ToTitleCase te gebruiken
$textInfo = (Get-Culture).TextInfo

# Voorbeeld van titelkapitalisatie
$titleCaseString = $textInfo.ToTitleCase("hello, powershell aficionados!")
Write-Output $titleCaseString

# Uitvoer: Hallo, Powershell Aficionados!

# Voorbeeld van bovenkapitaal
$upperCaseString = "make me shout".ToUpper()
Write-Output $upperCaseString

# Uitvoer: MAKE ME SHOUT

# Voorbeeld van benedenkapitaal
$lowerCaseString = "SILENCE IS GOLDEN".ToLower()
Write-Output $lowerCaseString

# Uitvoer: silence is golden
```

## Diep Duiken
Kapitalisatie komt uit de typografische traditie, waar titels en eigennamen beginnen met hoofdletters. In computerprogrammering is deze praktijk geïntroduceerd voor visuele standaardisatie en leesbaarheid.

Technisch gezien gaat `.ToTitleCase` niet alleen over het maken van letters in hoofdletters. Het volgt regels, zoals het niet kapitaliseren van voegwoorden, voorzetsels of artikelen in sommige contexten. Dat had je niet verwacht van een regel code, hè?

Er bestaan alternatieven: regex kan funky hoofdlettertransformaties doen, maar het is overkill voor eenvoudige taken. Plus, leesbaarheid telt—`.ToTitleCase`, `.ToUpper()`, en `.ToLower()` vertellen je precies wat ze doen. Geen giswerk nodig.

Een detail: wees voorzichtig met cultuurspecifieke regels die de kapitalisatie beïnvloeden. Bijvoorbeeld, "i" wordt "I" in het Engels, maar niet zo in andere talen. Hier schittert `TextInfo`; het respecteert culturele nuances.

## Zie Ook
Bekijk deze bronnen voor een diepere duik:

- [Microsoft Docs over ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
