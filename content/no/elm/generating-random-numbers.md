---
title:                "Elm: Generering av tilfeldige tall"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en vanlig oppgave i programmering, og det kan være nyttig for flere ulike bruksområder. Enten det er for å lage et mini-spill, generere tilfeldige brukere eller teste tilfeldige situasjoner, er det viktig å ha god forståelse for hvordan man kan gjøre dette i Elm.

## Slik gjør du det

For å generere tilfeldige tall i Elm, kan du bruke funksjonen `Random.float`:
```Elm
import Random

-- Generer et tilfeldig tall mellom 0 og 1
Random.float 0 1
```

Hvis du ønsker å generere et heltall, kan du bruke funksjonen `Random.int`:
```Elm
import Random

-- Generer et tilfeldig heltall mellom 1 og 10
Random.int 1 10
```

For å få ulike resultater hver gang, kan du bruke `Random.step`:
```Elm
import Random

-- Generer et tilfeldig heltall mellom 1 og 10, men få et nytt tall hver gang du kaller denne funksjonen
Random.step (Random.int 1 10) Random.initialSeed
```

## Dykk dypere

I Elm er tilfeldighet basert på en type kalt `Random.Generator`, som bruker en tilstandsfunksjon `Random.State` for å generere forskjellige tall hver gang den kjøres. For å generere et tilfeldig tall, bruker `Random`-modulen `Utils` for å konvertere til en `Generator`. Denne typen funksjon er også avhengig av en seed, som er et tilfeldig tall som brukes som utgangspunkt for å generere nye tall.

For å forstå dette bedre, kan du lese mer om tilfeldighet og `Random`-modulen i Elm-dokumentasjonen [her](https://package.elm-lang.org/packages/elm/random/latest/Random).

## Se også

- [Elm-dokumentasjon om tilfeldighet](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Elm-tutorial om tilfeldighet](https://elmprogramming.com/generating-random-numbers-in-elm.html)