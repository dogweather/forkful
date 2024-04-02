---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:25.156789-07:00
description: "Reguliere expressies (regex) zijn patronen die gebruikt worden om tekencombinaties\
  \ in strings te vinden. Programmeurs gebruiken ze voor het zoeken,\u2026"
lastmod: '2024-03-13T22:44:50.714590-06:00'
model: gpt-4-0125-preview
summary: "Reguliere expressies (regex) zijn patronen die gebruikt worden om tekencombinaties\
  \ in strings te vinden. Programmeurs gebruiken ze voor het zoeken,\u2026"
title: Reguliere expressies gebruiken
weight: 11
---

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die gebruikt worden om tekencombinaties in strings te vinden. Programmeurs gebruiken ze voor het zoeken, bewerken of manipuleren van tekst, wat taken zoals formuliervalidatie of gegevensverwerking vereenvoudigt.

## Hoe te gebruiken:

Elm heeft geen ingebouwde regex-mogelijkheden, maar je kunt het pakket `elm/regex` gebruiken. Hier is hoe je regex voor veelvoorkomende taken gebruikt:

```Elm
import Regex exposing (..)

-- Voorbeelden van regex-gebruik in Elm --

-- Controleren of een string "hallo" bevat
checkForHello : String -> Bool
checkForHello input =
    let
        patroon = "hallo"
        regex = Regex.fromString patroon |> Maybe.withDefault (regex ".")
    in
    Regex.contains regex input

-- Voorbeelduitvoer
checkForHello "hallo, wereld!" -- Waar

-- Cijfers uit een string halen
extractDigits : String -> List String
extractDigits input =
    let
        regex = Regex.fromString "\\d+" |> Maybe.withDefault (regex ".")
    in
    Regex.find (All) regex input |> List.map .match

-- Voorbeelduitvoer
extractDigits "elm123rocks" -- ["123"]
```
Onthoud, je moet Maybe behandelen voor mogelijke patroon-zoekfalen wanneer je `Regex.fromString` gebruikt.

## Diepere Duik

Regex gaat terug tot de jaren 1950, met wortels in de automaat theorie en formele taaltheorie. Door de tijd heen werd regex een krachtig hulpmiddel in tekstverwerking, geïntegreerd in vele programmeertalen en command-line hulpprogramma's.

Alternatieven voor regex in Elm omvatten stringfuncties zoals `String.contains`, `String.startsWith`, `String.split`, enz. Hoewel eenvoudiger, zijn ze minder krachtig voor complexe patroonherkenning.

Implementatiegewijs, is regex in Elm gebouwd op JavaScript's regex-motor, met dank aan Elm's runtime. Dit betekent dat het gedrag van regex in Elm de mogelijkheden en beperkingen van JavaScript kan weerspiegelen.

## Zie Ook

- Elm Regex Pakket: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Reguliere Expressies in JavaScript: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex Testers en Debuggers: [regex101.com](https://regex101.com)
