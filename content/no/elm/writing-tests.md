---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester er å lage kode som bekrefter at annen kode oppfører seg som forventet. Programmerere gjør dette for å avdekke feil tidlig, sikre programkvalitet og forenkle videreutvikling.

## Hvordan:

Elm bruker `elm-test` for enhetstesting. Her er et eksempel:

```Elm
import Expect
import Test exposing (..)
import ExampleProgram exposing (add)

suite : Test
suite =
    describe "ExampleProgram"
        [ test "add function adds two numbers correctly" <|
            \_ ->
                3 + 2
                    |> Expect.equal 5
        ]

-- Kjør tester med: elm-test
```

Resultat:
```
---- ExampleProgram Tests -------------------------------------------------------
✅ add function adds two numbers correctly

TEST RUN PASSED

1 test run, all passed!
```

## Dybdeanalyse

Elm-test ble lansert rundt 2016 og er fortsatt den ledende Elm-testrammeverket. Alternativer som `elm-check` for eiendomstesting eksisterer. Elm-tester utføres i isolasjon og fokuserer på ren funksjonell logikk, noe som gjør sideeffekt-testing utfordrende, og kan kreve tilleggsverktøy eller mønstre.

## Se Også:

- Elm-test pakken: [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- Elm-programmeringsspråkets offisielle side: [Elm-lang](https://elm-lang.org/)
- En artikkel om testdrevet utvikling (TDD) med Elm: [TDD i Elm](https://medium.com/@_rchaves_/test-driven-development-in-elm-1c28aa52e29a)
