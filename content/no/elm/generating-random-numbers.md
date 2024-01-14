---
title:    "Elm: Generering av tilfeldige tall"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall kan være nyttig for mange programmeringsoppgaver, som å lage spill eller simulere tilfeldige situasjoner. Det kan også være nyttig i statistikk og forskning.

## Hvordan lage tilfeldige tall i Elm

For å generere tilfeldige tall i Elm, kan du bruke funksjonen `Random.generate`. Denne funksjonen tar to argumenter: en funksjon for å generere tilfeldige tall og en funksjon for å behandle det genererte tallet. Se eksempelet nedenfor for å få en bedre forståelse.

```Elm
import Random

-- Definerer en funksjon for å generere tilfeldige tall mellom 1 og 10
randomNumber : Random.Generator Int
randomNumber =
  Random.int 1 10

-- Definerer en funksjon for å behandle det genererte tallet
-- i dette tilfellet skriver vi det ut i konsollen
printNumber : Int -> Cmd msg
printNumber number =
  Debug.log "Tilfeldig tall:" (toString number)

-- Genererer et tilfeldig tall og behandler det
randomCommand : Cmd msg
randomCommand =
  Random.generate printNumber randomNumber
```

Kjører eksempelet vil gi ulike tilfeldige tall hver gang.

## Dypdykk

Bak kulissene bruker Elm en algoritme kalt Mersenne Twister for å generere tilfeldige tall. Denne algoritmen er ansett som en av de beste for tilfeldig tallgenerering og har en veldig stor periode, noe som gjør at tallene er mer uforutsigbare.

## Se også

- [Elm dokumentasjon om tilfeldige tall](https://package.elm-lang.org/packages/elm/random/latest/)
- [Mersenne Twister algoritmen](https://en.wikipedia.org/wiki/Mersenne_Twister)