---
aliases:
- /nl/haskell/extracting-substrings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:49.295240-07:00
description: "Substringen extraheren betekent specifieke delen van een string naar\
  \ voren halen. Programmeurs doen dit om data te isoleren, te schonen, of om met\
  \ delen\u2026"
lastmod: 2024-02-18 23:09:01.886582
model: gpt-4-0125-preview
summary: "Substringen extraheren betekent specifieke delen van een string naar voren\
  \ halen. Programmeurs doen dit om data te isoleren, te schonen, of om met delen\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?

Substringen extraheren betekent specifieke delen van een string naar voren halen. Programmeurs doen dit om data te isoleren, te schonen, of om met delen te werken in plaats van het geheel.

## Hoe te:

In Haskell kun je strings in stukjes snijden met ingebouwde functies zoals `take`, `drop` en `substring` (van `Data.Text`).

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- Onze voorbeeldstring
let exampleStr = "Haskell makes sense!"

-- De eerste 7 karakters nemen
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- De eerste 8 karakters weggooien
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- Aangepaste functie om een substring te extraheren op positie en lengte
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- "makes" extraheren (startend van positie 8, lengte 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

Voorbeelduitvoer:
```
"Haskell"
"makes sense!"
"makes"
```

## Diepgaand

Het extraheren van substringen is al lang onderdeel van Haskell. Aanvankelijk vertrouwde het op lijsten, aangezien strings lijsten van karakters zijn. De prestaties waren niet geweldig. Voer `Data.Text` in, met efficiënte stringbewerkingen.

Alternatieven omvatten lijstbewerkingen, regex en parsingbibliotheken. Lijstbewerkingen zijn eenvoudiger maar trager voor grote strings. Regex is krachtig maar overkill voor eenvoudige taken. Parsingbibliotheken zijn bedoeld voor complexe parsing maar kunnen ook substrings aan.

Het implementeren van een aangepaste substring functie in Haskell gaat eenvoudig met `take` en `drop` van `Data.Text`, wat snellere stringverwerking biedt dan op lijsten gebaseerde bewerkingen.

## Zie Ook

- De documentatie van de module `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Leer jezelf Haskell voor veel plezier! voor een gemakkelijke duik in Haskell strings: http://learnyouahaskell.com/starting-out#immutability
- Haskell in de praktijk voor praktische gebruikscases: http://book.realworldhaskell.org/read/
- De Haskell Wiki voor inzichten van de gemeenschap: https://wiki.haskell.org/How_to_work_with_strings
