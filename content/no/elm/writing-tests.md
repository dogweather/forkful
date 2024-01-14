---
title:                "Elm: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester kan være en viktig del av utviklingsprosessen for å sikre at koden vår fungerer som den skal og for å unngå feil og bugs. Det kan også gjøre det enklere å vedlikeholde og videreutvikle koden senere.

## Hvordan

Vi kan skrive tester i Elm på en veldig enkel måte ved å bruke `elm-test` biblioteket. La oss se på et eksempel:

```Elm
module CalculatorTest exposing (..)

import Test
import Expect
import Calculator exposing (add)

addTest =
    Test.test "Adding positive numbers" <|
        \() ->
            Expect.equal (add 5 2) 7
```

Dette eksemplet tester funksjonen `add` i `Calculator`-modulen ved å legge sammen to positive tall og sammenligne resultatet med forventet output. Testen består hvis de to verdiene er like.

Nå kan vi kjøre testen ved å skrive følgende kommando i terminalen:

`elm-test CalculatorTest.elm`

Hvis testen består, får vi følgende output:

```
...
6 of 6 PASS

Success! Passed all 6 tests.
```

## Dypdykk

Når vi skriver tester i Elm, er det viktig å følge god praksis for å få mest mulig ut av dem. Noen tips for å skrive gode tester i Elm inkluderer:

- Skriv små og spesifikke tester som fokuserer på én funksjon eller ett aspekt av koden
- Skriv tester før du skriver koden for å sikre at du dekker alle mulige tilfeller og unngår feil og bugs
- Benytt deg av `Expect`-funksjonene for å sjekke forskjellige forventede resultater
- Dekk både positive og negative situasjoner i testene dine for å sikre robusthet

Med disse tipsene kan du skrive effektive og pålitelige tester som vil gjøre utviklingsprosessen enklere og mer trygg.

## Se også

- [Elm-test dokumentasjon](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm-test GitHub repository](https://github.com/elm-explorations/test)
- [Elm-test tutorial](https://thoughtbot.com/blog/testing-in-elm-what-to-test-and-where-to-test-it)