---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:48.216931-07:00
description: "Hoe: Haskell gebruikt de `Data.Char` module om karakters te manipuleren.\
  \ De functie `toLower` verandert specifiek een enkel karakter naar een kleine\u2026"
lastmod: '2024-03-13T22:44:50.839353-06:00'
model: gpt-4-0125-preview
summary: Haskell gebruikt de `Data.Char` module om karakters te manipuleren.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
Haskell gebruikt de `Data.Char` module om karakters te manipuleren. De functie `toLower` verandert specifiek een enkel karakter naar een kleine letter. Je zult deze functie over een string heen mappen om deze volledig naar kleine letters om te zetten. Bekijk de code:

```haskell
import Data.Char (toLower)

-- Een string naar kleine letters omzetten
lowercaseString :: String -> String
lowercaseString = map toLower

-- Gebruik
main :: IO ()
main = putStrLn $ lowercaseString "Hello, Haskell!"
```

Voorbeelduitvoer:

```
hello, haskell!
```

## Dieper Duiken
Historisch gezien komt het concept van lettercases uit het tijdperk van handmatige letterzetting, toen hoofdletters en kleine letters in aparte bakken werden bewaard. In programmering zorgt de omzetting van hoofdletters naar kleine letters voor uniformiteit, vooral bij hoofdletterongevoelige bewerkingen.

Hier is de DL over Haskell bijzonderheden. De `Data.Char` module, met daarin `toLower`, verscheen in de Haskell 98 standaard. Sindsdien is het de goto voor karaktermanipulaties. Andere talen hebben hun eigen methoden, zoals `.toLowerCase()` in JavaScript of `.lower()` in Python, maar in Haskell doen `map` en `toLower` het werk netjes.

Onder de motorkap houdt `toLower` rekening met Unicode, wat betekent dat het een breed repertoire aan karakters en scripts kan verwerken, ver buiten het basis ASCII bereik – handig voor internationalisatie.

Alternatieven? Zeker, je zou je eigen functie kunnen uitrollen die `toLower` nabootst, maar waarom het wiel opnieuw uitvinden? Blijf bij `Data.Char` voor leesbaarheid en betrouwbaarheid. Plus, bibliotheken zoals `text` en `bytestring` bieden meer prestatiegerichte benaderingen als je met grote datasets werkt of streeft naar prestatie.

## Zie Ook
- `Data.Char` documentatie: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Haskell 98 Rapport over `Data.Char`: https://www.haskell.org/onlinereport/standard-prelude.html
- Text bibliotheek voor Haskell: https://hackage.haskell.org/package/text
- ByteString bibliotheek voor Haskell: https://hackage.haskell.org/package/bytestring
