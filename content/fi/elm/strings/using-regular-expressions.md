---
title:                "Säännöllisten lausekkeiden käyttö"
aliases: - /fi/elm/using-regular-expressions.md
date:                  2024-02-03T19:16:49.350347-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet (regex) ohjelmoinnissa ovat malleja, joita käytetään merkkiyhdistelmien vastaavuuden tarkistamiseen merkkijonoissa. Elm:ssä, kuten muissakin ohjelmointikielissä, ohjelmoijat käyttävät regexiä tehtäviin kuten syötteen validointi, etsintä ja tekstin korvaaminen merkkijonoissa niiden joustavuuden ja tehokkuuden vuoksi.

## Kuinka:
Elm ei sisällä sisäänrakennettuja regex-funktioita sen ydinkirjastossa, joten näiden operaatioiden suorittamiseen tarvitaan kolmannen osapuolen kirjastoja. Yksi suosittu valinta regexin kanssa työskentelyyn on `elm/regex`. Voit lisätä sen projektiisi käyttäen `elm install elm/regex`.

Tässä on, miten voit käyttää `elm/regex`iä muutamassa yleisessä tehtävässä:

### 1. Mallin vastaavuuden tarkistaminen
Tarkistaaksesi, vastaako merkkijono mallia, voit käyttää `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Esimerkin käyttö:
isAlphanumeric "Elm2023"     -- Tuloste: True
isAlphanumeric "Elm 2023!"   -- Tuloste: False
```

### 2. Kaikkien vastaavuuksien löytäminen
Löytääksesi kaikki mallin esiintymät merkkijonossa, voit käyttää `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Esimerkin käyttö:
getWords "Elm on kivaa!"  -- Output: ["Elm", "on", "kivaa"]
```

### 3. Tekstin korvaaminen
Korvataksesi osia merkkijonosta, jotka vastaavat mallia, käytä `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Esimerkin käyttö:
replaceElmWithHaskell "Elmin oppiminen on kivaa!"  
-- Tuloste: "Haskellin oppiminen on kivaa!"
```

Näissä esimerkeissä `Regex.fromString` käytetään regex-mallin kokoamiseen, jossa `\b` vastaa sanarajoja ja `\w` vastaa mitä tahansa sanamerkkiä. Käsittele aina `Regex.fromString` tuloksena saatu `Maybe` varmistaaksesi, että et joudu invalidin regex-mallin kanssa ongelmiin, tyypillisesti käyttämällä `Maybe.withDefault`.
