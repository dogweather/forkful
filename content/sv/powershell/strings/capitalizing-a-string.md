---
title:                "Gör om en sträng till versaler"
date:                  2024-02-03T19:06:04.451254-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav i PowerShell innebär att man omvandlar det första tecknet i en given sträng till versal medan resten av strängen lämnas oförändrad. Programmerare utför ofta denna uppgift för formateringssyften, såsom att förbereda text för visning i användargränssnitt eller följa grammatiska regler i genererade dokument.

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
