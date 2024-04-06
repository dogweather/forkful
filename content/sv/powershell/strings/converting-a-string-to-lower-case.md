---
date: 2024-01-20 17:39:02.398702-07:00
description: "Hur g\xF6r man: Resultat."
lastmod: '2024-04-05T21:53:39.448492-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

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
