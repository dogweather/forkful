---
title:                "Konvertera en sträng till gemener"
aliases:
- sv/powershell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:02.398702-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener betyder att ändra alla stora bokstäver till små. Programmerare gör detta för att standardisera data och underlätta jämförelser, särskilt när skiftlägeskänslighet kan leda till problem.

## Hur gör man:
```PowerShell
# Konvertera en sträng till gemener
$sträng = "Hej VÄRLDEN!"
$småBokstäverSträng = $sträng.ToLower()

# Utskrift
$småBokstäverSträng
```

Resultat:
```
hej världen!
```

## Fördjupning
På de tidiga dagarna av databehandling blev det snabbt tydligt att jämförelser och sorteringar behövde standardisering. Därför skapades metoder för att konvertera till gemener. Alternativ till `.ToLower()` kan inkludera `.ToUpper()` för att göra motsatsen, eller reguljära uttryck för mer komplexa scenarier. Detta fungerar genom att mappa varje tecken i en sträng till deras gemena motsvarigheter enligt teckenkodningstabellen som används av systemet, ofta Unicode.

## Se även
- [PowerShell documentation on String.ToLower() method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Unicode case mapping](https://unicode.org/reports/tr21/)
