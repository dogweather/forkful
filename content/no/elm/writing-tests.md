---
title:                "Skriving av tester"
html_title:           "Elm: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle man bry seg med å skrive tester, når man allerede har tilbrakt timer med å skrive og feilsøke koden? Vel, her er noen gode grunner til hvorfor det kan være verdt innsatsen:

- Å skrive tester hjelper deg med å fange feil tidlig i koden, noe som betyr mindre arbeid senere når koden skal integreres og testes som en helhet.
- Det er også en god måte å dokumentere koden din på, og gjøre det enklere for andre å forstå hva du har skrevet.

## Hvordan

Å skrive tester i Elm er enkelt og intuitivt. Her er et eksempel på å skrive en enkel test for en funksjon som legger sammen to tall og sjekker om resultatet er riktig:

```Elm 
import Test exposing (..)

addNumbers : Int -> Int -> Int
addNumbers x y =
    x + y

testAddNumbers : Test
testAddNumbers =
    describe "Adding numbers"
        [ test "1 + 1 should be 2" <|
            \_ -> 
                Expect.equal (addNumbers 1 1) 2
        ]
```

Som du kan se, bruker vi funksjonen `expect` for å sjekke om resultatet er lik det vi forventer. Når du kjører testen, vil det enten produsere en grønn eller rød melding, avhengig av om testen består. Du kan også kjøre flere tester ved å bruke funksjonen `testList` og gruppere dem sammen.

## Dykk dypere

Hvis du ønsker å lære mer om hvordan du kan skrive tester i Elm, kan du sjekke ut Elm sin offisielle dokumentasjon om Testing. Der vil du finne mer detaljert informasjon om ulike typer tester, debugging og vanlige feil du kan støte på.

## Se også

- [Offisiell Elm Testing dokumentasjon](https://guide.elm-lang.org/testing/)
- [En oversikt over Elm Testing verktøy](https://medium.com/@felixblaschke/testing-elm-apps-an-overview-of-tools-52b0a93709c1)
- [Elm Test Tutorial fra Scrimba](https://scrimba.com/g/gelmtesting)