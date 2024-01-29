---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:55:42.955500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het kapitaliseren van een string betekent het converteren van het eerste karakter naar een hoofdletter en de rest in kleine letters houden. Programmeurs doen dit voor eigennamen, titels of om een consistente stijl af te dwingen voor koppen en tekstinhoud.

## Hoe:
In Elm krijg je niet standaard een kapitaliseerfunctie, maar je kunt er gemakkelijk een maken:

```Elm
import String exposing (toUpper, toLower, left, dropLeft)

capitalize : String -> String
capitalize tekst =
    if String.isEmpty tekst then
        ""
    else
        toUpper (left 1 tekst) ++ toLower (dropLeft 1 tekst)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Uitvoer: "Hello Elm World"
```

## Diepgaande Duik
Elm geeft de voorkeur aan explicietheid en bevat geen veelvoorkomende stringmanipulaties zoals `capitalize` in de kernbibliotheken. Historisch gezien maak je ofwel je eigen oplossing of haal je een externe bibliotheek binnen die `String` manipulaties uitbreidt.

Elm's kern `String` bibliotheek biedt `toUpper` en `toLower`, die volledige stringtransformaties afhandelen. Om te kapitaliseren, neem je het eerste karakter met `left`, zet je het om naar een hoofdletter met `toUpper`, en dan voeg je het toe aan de rest van de string, omgezet in kleine letters door `toLower`. Het resterende deel van de string wordt geëxtraheerd met behulp van `dropLeft`, wat voorkomt dat het eerste karakter wordt beïnvloed.

Hoewel Elm's standaardbibliotheken mogelijk geen native `capitalize` functie bevatten, zorgt de beslissing voor een minimalistische en performante kern, waardoor dergelijke specifieke hulpprogramma's aan de implementatie van de gebruiker of aanvullende pakketten worden overgelaten.

Alternatieven omvatten het gebruik van uitgebreide stringmanipulatiepakketten zoals `elm-string-extra`, die een `capitalize` functie bevatten, naast andere handige stringoperaties:

```Elm
import String.Extra exposing (capitalize)

main =
    String.words "hello elm world" |> List.map capitalize |> String.join " "
    -- Uitvoer: "Hello Elm World"
```

Merk op dat Elms benadering van strings zich bewust is van Unicode, wat betekent dat het kapitalisatie correct afhandelt, zelfs voor talen met niet-Latijnse alfabetten, zij het met extra complexiteiten.

## Zie Ook
- Elm `String` documentatie: https://package.elm-lang.org/packages/elm/core/latest/String
- `elm-string-extra` bibliotheek op Elm pakketten: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- Unicode-standaard voor hoofdletterconversies: https://www.unicode.org/reports/tr21/
