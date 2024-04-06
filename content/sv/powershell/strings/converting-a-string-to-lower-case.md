---
date: 2024-01-20 17:39:02.398702-07:00
description: "Hur g\xF6r man: P\xE5 de tidiga dagarna av databehandling blev det snabbt\
  \ tydligt att j\xE4mf\xF6relser och sorteringar beh\xF6vde standardisering. D\xE4\
  rf\xF6r skapades\u2026"
lastmod: '2024-04-05T22:50:52.416995-06:00'
model: gpt-4-1106-preview
summary: "P\xE5 de tidiga dagarna av databehandling blev det snabbt tydligt att j\xE4\
  mf\xF6relser och sorteringar beh\xF6vde standardisering."
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
