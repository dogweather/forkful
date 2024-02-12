---
title:                "Een string met hoofdletters maken"
aliases:
- /nl/haskell/capitalizing-a-string/
date:                  2024-01-28T21:55:30.319519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het kapitaliseren van een string betekent het corrigeren van de hoofdletters zodanig dat de eerste letter een hoofdletter is en de rest kleine letters zijn. Programmeurs doen dit voor consistentie, leesbaarheid en om te voldoen aan data-opmaaknormen.

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
