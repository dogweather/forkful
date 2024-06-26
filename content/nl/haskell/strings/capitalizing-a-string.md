---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:30.319519-07:00
description: "Hoe: Om strings in Haskell te kapitaliseren, heeft de taal zelf geen\
  \ ingebouwde `capitalize` functie. Daarom maken we er zelf een met behulp van de\u2026"
lastmod: '2024-03-13T22:44:50.835475-06:00'
model: gpt-4-0125-preview
summary: Om strings in Haskell te kapitaliseren, heeft de taal zelf geen ingebouwde
  `capitalize` functie.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe:
Om strings in Haskell te kapitaliseren, heeft de taal zelf geen ingebouwde `capitalize` functie. Daarom maken we er zelf een met behulp van de `toUpper` en `toLower` functies van de `Data.Char` module.

```Haskell
import Data.Char (toUpper, toLower)

-- Maakt de eerste letter van een string een hoofdletter en de rest kleine letters
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

main = do
  print $ capitalize "haskell"       -- Geeft "Haskell"
  print $ capitalize "hASKELL"       -- Geeft "Haskell"
  print $ capitalize ""              -- Geeft ""
  print $ capitalize "hello world!"  -- Geeft "Hello world!"
```

## Diepgaande Duik
Haskell, een functionele programmeertaal, bevat geen eenvoudige stringkapitalisatie in zijn standaardbibliotheek, mogelijk omdat het triviaal te implementeren is en niet vaak nodig is voor het type programmering waarvoor het is ontworpen.

Alternatieven voor de `capitalize` functie zouden `Data.Text` kunnen gebruiken, wat prestatievoordelen kan bieden voor grote teksten vanwege efficiëntere interne voorstellingen. Of kijk naar bibliotheken zoals `text-icu` voor robuuste locatiegevoelige kapitalisatie.

Wat betreft de implementatie, is het de moeite waard op te merken dat onze `capitalize` functie niet omgaat met niet-ASCII karakters. Als je volledige Unicode-ondersteuning nodig hebt, zou je moeten kijken naar een bibliotheekoplossing of complexe gevallen van Unicode-kapitalisatie moeten afhandelen waarbij eenvoudige teken-voor-teken transformaties niet volstaan.

## Zie Ook
- Haskell's `Data.Char` module: http://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- `Data.Text` voor efficiënte tekstmanipulatie: http://hackage.haskell.org/package/text
- Introductie tot tekstverwerking in Haskell: https://wiki.haskell.org/Text_Processing
- Overwegingen met betrekking tot Unicode in Haskell: https://wiki.haskell.org/Unicode_input_and_output
