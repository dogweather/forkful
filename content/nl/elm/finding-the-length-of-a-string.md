---
title:                "De lengte van een string vinden"
date:                  2024-01-28T22:00:00.797049-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string bepalen betekent het tellen van de karakters. Programmeurs doen dit om invoer te valideren, tekst te manipuleren of simpelweg om data te bemeten.

## Hoe doe je dat:
In Elm gebruik je `String.length` om te achterhalen hoeveel karakters een string bevat. Kijk maar:

```elm
import Html exposing (text)

main =
  text (String.fromInt (String.length "Hallo, Elm!"))
  -- Output: "11"
```

## Diep Duiken
Historisch gezien zijn functies voor het bepalen van de lengte van strings cruciaal geweest voor geheugenbeheer en tekstverwerking in talen met laag-niveau toegang tot data. Elm, als een hoog-niveau taal, abstraheert deze details, en biedt ingebouwde functionaliteit met `String.length`.

Twee punten die de moeite waard zijn om te noteren:
1. Elm strings zijn UTF-16 gecodeerd. `String.length` geeft het aantal UTF-16 code-eenheden terug, wat kan verschillen van het daadwerkelijke aantal Unicode grafemen (door de gebruiker waargenomen karakters) in strings met complexe karakters.
2. Er zijn geen ingebouwde alternatieven voor `String.length` in Elm. Als je het aantal grafemen nodig hebt, heb je misschien een aangepaste functie nodig die rekening houdt met de complexiteit van Unicode.

Intern doorloopt `String.length` de datastructuur van de string, waarbij elementen geteld worden. Als een zuivere functie hangt zijn output uitsluitend van de invoer af, waardoor het functioneel programmeringsethos van Elm behouden blijft.

## Zie Ook
- De officiële `String` documentatie van Elm: [https://package.elm-lang.org/packages/elm/core/latest/String#length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- UTF-16: [https://nl.wikipedia.org/wiki/UTF-16](https://nl.wikipedia.org/wiki/UTF-16)
