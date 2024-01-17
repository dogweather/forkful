---
title:                "Arbeide med yaml"
html_title:           "Haskell: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med YAML handler om å organisere og lagre informasjon på en enkel og lesbar måte. Det brukes ofte av programmerere for å lagre konfigurasjonsdata og strukturere komplekse datastrukturer. 

## Slik gjør du det:

Her er et eksempel på hvordan du kan lage en YAML-fil og lese den i Haskell:

```Haskell
-- For å jobbe med YAML, må vi importere biblioteket
import Data.Yaml

-- Definer en datastruktur, i dette tilfellet en liste av tall
data Numbers = Numbers [Int] deriving (Show, Generic) 

-- Lagre dataen som YAML i en fil
let data = Numbers [1, 2, 3]
encodeFile "numbers.yml" data

-- Les dataen fra YAML-filen og skriv ut
numbers <- decodeFileThrow "numbers.yml" :: IO Numbers
print numbers

-- Output: Numbers [1, 2, 3]
```

## Dypdykk:

YAML (Yet Another Markup Language) dukket opp i 2001 og er inspirert av programmeringsspråket Python. Det brukes ofte som et alternativ til JSON for å representere kompleks data, og er spesielt nyttig for å konfigurere komplekse systemer. I Haskell, brukes biblioteket "Data.Yaml" for å enkelt arbeide med YAML-data. 

## Se også:

- [Offisiell YAML-nettside] (https://yaml.org/)
- [Haskell Yaml bibliotek] (https://hackage.haskell.org/package/yaml)