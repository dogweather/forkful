---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:38.216806-07:00
description: "Substringen extraheren betekent specifieke delen uit een string halen.\
  \ Programmeurs doen dit om stukken tekstgegevens te isoleren, te manipuleren of\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.713675-06:00'
model: gpt-4-0125-preview
summary: "Substringen extraheren betekent specifieke delen uit een string halen. Programmeurs\
  \ doen dit om stukken tekstgegevens te isoleren, te manipuleren of te\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?
Substringen extraheren betekent specifieke delen uit een string halen. Programmeurs doen dit om stukken tekstgegevens te isoleren, te manipuleren of te analyseren.

## Hoe te:

Elm maakt het gemakkelijk. Laten we beginnen met `String.slice`:

```Elm
import String exposing (slice)

volledigeTekst : String
volledigeTekst = "Hallo, Elm wereld!"

-- Het extraheren van "Elm"
subtekenreeks : String
subtekenreeks = slice 7 10 volledigeTekst

-- Uitvoer: "Elm"
```

Laten we nu iets dynamischer worden met `String.left` en `String.right`:

```Elm
import String exposing (left, right)

-- De eerste 5 tekens krijgen
linkseTekst : String
linkseTekst = left 5 volledigeTekst

-- Uitvoer: "Hallo"

-- De laatste 5 tekens krijgen
rechtseTekst : String
rechtseTekst = right 5 volledigeTekst

-- Uitvoer: "orld!"
```

## Diepgaand

Historisch gezien is het extraheren van substrings zo oud als het programmeren zelf. In Elm, zoals in andere functionele talen, zijn de functies voor het manipuleren van strings onveranderlijk - ze retourneren nieuwe strings in plaats van het origineel te veranderen.

Er bestaan alternatieven zoals `String.dropLeft` en `String.dropRight`. Deze trimmen karakters van één van de uiteinden van de string:

```Elm
import String exposing (dropLeft, dropRight)

-- De eerste 7 tekens verwijderen
verwijderdLinksTekst : String
verwijderdLinksTekst = dropLeft 7 volledigeTekst

-- Uitvoer: "Elm wereld!"

-- De laatste 6 tekens verwijderen
verwijderdRechtsTekst : String
verwijderdRechtsTekst = dropRight 6 volledigeTekst

-- Uitvoer: "Hallo, Elm"
```

Wat betreft implementatie, deze functies zijn ingebouwd in de Elm standaardbibliotheek en gaan om met Unicode, hoewel er overwegingen moeten worden gemaakt met betrekking tot Unicodes surrogate paren en combinerende karakters.

## Zie Ook

- Elm `String` module documentatie: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm gids over strings: https://guide.elm-lang.org/strings/
- MDN Web Docs over Unicode: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
