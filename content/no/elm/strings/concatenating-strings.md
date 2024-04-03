---
date: 2024-01-20 17:34:36.708009-07:00
description: "How to: Elm gj\xF8r det lett \xE5 sl\xE5 sammen strenger med `++` operatoren.\
  \ Her er et eksempel."
lastmod: '2024-03-13T22:44:40.700457-06:00'
model: gpt-4-1106-preview
summary: "Elm gj\xF8r det lett \xE5 sl\xE5 sammen strenger med `++` operatoren."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## How to:
Elm gjør det lett å slå sammen strenger med `++` operatoren. Her er et eksempel:

```Elm
main =
  let
    greeting = "Hei, "
    name = "Ola"
  in
    text (greeting ++ name ++ "!")
```

Kjører du dette, vil utdataen bli `Hei, Ola!`

## Deep Dive
Tilbake i tiden var sammensmelting av strenger litt av et styr siden man måtte holde øye med minne og ytelse. Moderne språk som Elm tar seg av dette for deg. Elm bruker `(++)` operator til å slå sammen strenger, og det gjør koden ren og lett. Men det er greit å være oppmerksom på at for store mengder string konkatenasjon kan være tregt, fordi Elm må traversere hele strengen hver gang.

Det er også alternativer for å slå sammen strenger:
- `String.concat` tar en liste med strenger og smelter dem sammen.
- `String.join` slår sammen en liste med strenger med en delimiter.

Her er et eksempel:

```Elm
main =
  let
    words = [ "Elm", "er", "gøy!" ]
  in
    text (String.join " " words)
```

Dette vil også gi deg `Elm er gøy!`

## See Also
Ta en titt på Elm sin offisielle dokumentasjon for mer detaljer:
- [String - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- Lær mer om funksjonell programmering [Haskell.org](http://www.haskell.org) (Elm er inspirert av Haskell).
- [Elm Guide - Text](https://guide.elm-lang.org/core_language.html) for en intro til strenger og mer.
