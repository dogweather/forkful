---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:04.451254-07:00
description: "Hur g\xF6r man: PowerShell, som \xE4r ett m\xE5ngsidigt verktyg, l\xE5\
  ter dig skriva med stor bokstav i en str\xE4ng p\xE5 enkla s\xE4tt utan att beh\xF6\
  va tredjepartsbibliotek.\u2026"
lastmod: '2024-03-13T22:44:38.107489-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, som \xE4r ett m\xE5ngsidigt verktyg, l\xE5ter dig skriva med\
  \ stor bokstav i en str\xE4ng p\xE5 enkla s\xE4tt utan att beh\xF6va tredjepartsbibliotek."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur gör man:
PowerShell, som är ett mångsidigt verktyg, låter dig skriva med stor bokstav i en sträng på enkla sätt utan att behöva tredjepartsbibliotek. Så här kan du göra det:

```powershell
# Använder den inbyggda .Net-metoden 'ToTitleCase' från CultureInfo
$text = "hej värld"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Utskrift:
```
Hej värld
```

Notera: Denna metod gör den första bokstaven i varje ord versal. Om du strikt vill göra endast det första tecknet i strängen versalt och lämna resten som det är, kan du göra något i den här stilen:

```powershell
# Gör endast det första tecknet i en sträng till versal
$text = "hej värld"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Utskrift:
```
Hej värld
```

PowerShell inkluderar inte direkt en enkel funktion för att göra endast det första bokstaven i en sträng versal, men genom att kombinera grundläggande strängmanipulationsmetoder såsom `Substring(0,1).ToUpper()` och sammanfogning kan vi lätt uppnå det önskade resultatet.
