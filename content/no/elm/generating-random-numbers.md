---
title:    "Elm: Generering av tilfeldige tall"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere har behov for å generere tilfeldige tall i sine programmer. Dette kan være for å lage spill, teste algoritmer eller til og med for å legge til litt tilfeldighet i en applikasjon. Elm, et programmeringsspråk som fokuserer på funksjonell programmering, tilbyr flere innebygde funksjoner for å generere tilfeldige tall. I denne bloggposten vil vi se på hvorfor og hvordan man kan bruke disse funksjonene.

## Hvordan

For å generere tilfeldige tall i Elm, kan du bruke funksjonen `Random.generator`, som tar inn en `Random.Generator` som argument. Denne generator funksjonen er bygd opp av kombinasjonen av forskjellige handlinger og tall som deretter kan brukes til å generere nye tall.

For eksempel, hvis vi ønsker å generere et tilfeldig tall mellom 1 og 10, kan vi bruke følgende kode:

```elm
import Random exposing (Generator, int)
import Random.List exposing (shuffle)

numbers : List Int
numbers = [1..10]

tallGenerator : Generator Int
tallGenerator = shuffle numbers |> andThen int 1 10
```

I dette eksemplet bruker vi funksjonene `Random.int` og `Random.List.shuffle` for å lage en generator som først blander tallene i listen `numbers`, og deretter genererer et tilfeldig tall mellom 1 og 10.

Når du har en generator, kan du bruke den til å generere tilfeldige tall ved å bruke funksjonen `Random.generate`, som tar inn en `Msg` funksjon og en generator. Her er et eksempel på hvordan du kan bruke vår `tallGenerator` til å generere tilfeldige tall og lagre dem i en variabel:

```elm
randomTall : Int
randomTall =
  Random.generate RandomTall tallGenerator

type Msg = RandomTall Int
```

Nå vil `randomTall` variabelen inneholde et tilfeldig tall hver gang du kjører funksjonen.

## Dypdykk

Funksjonene vi har brukt så langt, `Random.generator` og `Random.generate`, er bare to av de mange mulighetene for å generere tilfeldige tall i Elm. Det er også andre funksjoner som `Random.bool` for å generere tilfeldige boolske verdier og `Random.float` for å generere tilfeldige desimaltall.

Du kan også bygge dine egne generatorer ved å kombinere forskjellige funksjoner og handlinger, noe som gir enda mer fleksibilitet til å generere tilfeldige tall som passer til din applikasjon.

## Se også

- [Offisiell dokumentasjon for generering av tilfeldige tall i Elm](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Elm programming language website](https://elm-lang.org/)
- [Elm for nybegynnere kurs på Udemy](https://www.udemy.com/course/elm-for-beginners/)

Takk for at du leste! Med disse verktøyene kan du enkelt legge til tilfeldighet i dine Elm-prosjekter. Fortsett å utforske og lære mer om de spennende mulighetene som funksjonell programmering tilbyr. Lykke til!