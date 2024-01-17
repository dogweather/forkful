---
title:                "Skriva tester"
html_title:           "Elm: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester är en viktig del av programmering, då det hjälper oss att säkerställa att vår kod fungerar korrekt och inte bryts när vi gör ändringar. Tester är små bitar av kod som testar specifika delar av vår programvara och ger oss feedback om de har passerat eller misslyckats. Programmerare gör det för att säkerställa att deras kod är tillförlitlig och uppvisar förväntade resultat.

## Hur man gör:

```Elm
import Test exposing (..)

-- En enkel funktion som returnerar sitt argument plus 1
addOne : Int -> Int
addOne x =
    x + 1

-- Ett test som kontrollerar att vår addOne-funktion returnerar förväntat resultat
testAddOne : () -> Test
testAddOne _ =
    describe "addOne"
        [ test "Returns expected result" <|
            \_ ->
                addOne 5
                |> Expect.equal 6 -- Verifierar att 5 + 1 är lika med 6
        ]

-- Kör testet och få ut resultatet
runTests : Test
runTests =
    describe "Our Test Suite"
        [ testAddOne -- Lägg till fler tester här om du vill
        ]

main =
    runTests
        |> toString
        |> text

```

När vi kör koden ovan får vi ut som förväntat: `OK, passed 1 test`.

## Djupdykning:

I början av programmeringens historia utfördes tester manuellt av programmerare. Det var inte förrän på 1960-talet som idén om automatiserade tester uppkom, vilket gjorde testprocessen mer effektiv. Alternativ till att skriva tester med Elm inkluderar JavaScripts Jest och Pythons Pytest. För att implementera tester i din kod behöver du använda Test.exposing-modulen och sedan skriva dina tester inuti en ```describe```-funktion.

## Se även:

- [Elms dokumentation om tester] (https://package.elm-lang.org/packages/elm-explorations/test/latest/) 
- [Jests dokumentation] (https://jestjs.io/docs/en/getting-started)
- [Pytests dokumentation] (https://docs.pytest.org/en/stable/)