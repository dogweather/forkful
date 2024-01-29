---
title:                "Karakters die overeenkomen met een patroon verwijderen"
date:                  2024-01-28T21:58:22.076204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters die overeenkomen met een patroon verwijderen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van tekens die overeenkomen met een patroon betekent het uitwissen van specifieke reeksen tekens uit tekst, gebaseerd op regels (patronen). Programmeurs doen dit voor het opschonen van tekst, gegevensverwerking, of om invoer te vereenvoudigen voor het parsen.

## Hoe:
Elm ondersteunt van nature geen regex, maar je kunt karakterverwijdering simuleren. Hier is een voorbeeld met `String.filter` om cijfers uit een string te verwijderen.

```Elm
importeer Browser
importeer Html die (text) blootlegt

verwijderCijfers : String -> String
verwijderCijfers = String.filter (\char -> not (char >= '0' && char <= '9'))

hoofd =
  text (verwijderCijfers "Elm 0.19.1 is super 123 cool!")

-- Uitvoer: "Elm . is super  cool!"
```

## Diepere Duik
Elm mist regex als onderdeel van zijn kern taal, in tegenstelling tot veel andere talen. Deze ontwerpkeuze is in lijn met Elm's doelen voor eenvoudigheid en veiligheid. Regex kan foutgevoelig en moeilijk te debuggen zijn, maar Elm pleit voor eenvoudigere stringoperaties die veelvoorkomende gebruikscases dekken.

Voor gevallen waarin regex echt nodig is, vertrouwt de implementatie op JavaScript interop via poorten. Echter, Elm stimuleert het vinden van oplossingen binnen de taal eerst. De `String` module biedt functies zoals `filter`, `replace`, en `split` die een breed scala aan patroongebaseerde tekstmanipulatie dekken zonder de complexiteit van regex te introduceren.

## Zie Ook
- [Elm String documentatie](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Praktisch Elm voor een Drukke Ontwikkelaar](https://korban.net/elm/book/) - Boek dat tekstmanipulatie-hulpprogramma's bevat.
