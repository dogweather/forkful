---
date: 2024-01-20 18:03:21.406676-07:00
description: "Et nytt prosjekt er starten p\xE5 en frisk kodebase, og programmerere\
  \ starter nye prosjekter for \xE5 realisere unike ideer eller l\xF8se spesifikke\
  \ problemer. Det\u2026"
lastmod: '2024-03-13T22:44:40.709376-06:00'
model: gpt-4-1106-preview
summary: "Et nytt prosjekt er starten p\xE5 en frisk kodebase, og programmerere starter\
  \ nye prosjekter for \xE5 realisere unike ideer eller l\xF8se spesifikke problemer.\
  \ Det\u2026"
title: "\xC5 starte et nytt prosjekt"
---

{{< edit_this_page >}}

## What & Why?
Et nytt prosjekt er starten på en frisk kodebase, og programmerere starter nye prosjekter for å realisere unike ideer eller løse spesifikke problemer. Det gir ren arkitektur og nytt potensial.

## How to:
Installer først `elm` på systemet ditt. Bruk `elm init` for å sette opp et nytt prosjekt:

```Elm
$ elm init
```

Du får nå en `elm.json` fil og en `src` mappe. For å lage din første Elm-modul:

```Elm
module Main exposing (main)

import Html exposing (text)

main = 
    text "Hei, Norge!"

```

Lagre koden over i en fil kalt `Main.elm` under `src` mappen. Kjør så:

```Elm
$ elm make src/Main.elm
```

Dette produserer en `index.html` som du kan åpne i nettleseren din for å se "Hei, Norge!".

## Deep Dive:
Elm ble introdusert i 2012, og har siden tilbudt et robust system for å lage webapplikasjoner. Med en kompilator som genererer JavaScript, unngår man runtime-feil, og får en sterk type-sikkerhet. Alternativer inkluderer språk som PureScript og ReasonML, som også sikter mot funksjonell programmering i frontend-utvikling. Siden Elm styrer hele view-laget, passer det godt i prosjekter hvor du vil unngå direkte manipulasjon av DOM og heller fokusere på robusthet og vedlikeholdbarhet.

## See Also:
- Elm's offisielle hjemmeside: [https://elm-lang.org/](https://elm-lang.org/)
- Elm pakker og dokumentasjon: [https://package.elm-lang.org/](https://package.elm-lang.org/)
- Elm diskusjonsforum: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
