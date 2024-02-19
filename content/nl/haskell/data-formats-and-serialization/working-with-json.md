---
aliases:
- /nl/haskell/working-with-json/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:24.960978-07:00
description: "JSON (JavaScript Object Notation) is een tekstgebaseerd gegevensformaat\
  \ voor het opslaan en transporteren van data. Programmeurs gebruiken het omdat het\u2026"
lastmod: 2024-02-18 23:09:01.922386
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) is een tekstgebaseerd gegevensformaat\
  \ voor het opslaan en transporteren van data. Programmeurs gebruiken het omdat het\u2026"
title: Werken met JSON
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON (JavaScript Object Notation) is een tekstgebaseerd gegevensformaat voor het opslaan en transporteren van data. Programmeurs gebruiken het omdat het lichtgewicht is, makkelijk te lezen/schrijven, en taalonafhankelijk.

## Hoe te:

In Haskell, gaan we om met JSON gebruikmakend van de `aeson` bibliotheek. Om te beginnen, importeer deze en definieer een type dat overeenkomt met je verwachte JSON-structuur.

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- Uitgaande van dat we een JSON-object hebben met een "naam" en een "leeftijd"

data Persoon = Persoon 
  { naam :: String
  , leeftijd  :: Int
  } afgeleid (Generic, Show)

instantie FromJSON Persoon
instantie ToJSON Persoon

-- JSON-string parsen
main :: IO ()
main = doe
  let jsonString = "{\"naam\":\"John\", \"leeftijd\":30}"
  let misschienPersoon = decodeer jsonString :: Misschien Persoon
  geval misschienPersoon van
    Niets -> putStrLn "Fout bij het parsen van JSON."
    Gewoon persoon -> print persoon
```

Uitvoer:
```
Persoon {naam = "John", leeftijd = 30}
```

## Diepduiken

- **Geschiedenis**: Het ontwerp van JSON werd beïnvloed door een subset van de JavaScript-syntax, en het won aan populariteit als een eenvoudig alternatief voor XML.
- **Alternatieven**: Hoewel JSON koning is voor web-API's, kunnen alternatieven zoals XML, YAML of zelfs Protocol Buffers worden gekozen op basis van context en vereisten.
- **Implementatiedetails**: `aeson` gebruikt het typesysteem van Haskell om JSON-structuren te matchen met Haskell-typen. Parsen wordt gedaan via typeklassen zoals `FromJSON`, en codering door `ToJSON`.

## Zie Ook

- `aeson` pakketdocumentatie: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
- Echte JSON-API's om mee te oefenen: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)
- JSON-specificatie: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
