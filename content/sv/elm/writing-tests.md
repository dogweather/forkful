---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapa tester innebär att skriva kod som kontrollerar att andra delar av din kod fungerar som de ska. Programmerare gör detta för att automatiskt upptäcka och förebygga fel, vilket sparar tid och ökar kodens pålitlighet.

## Hur man gör:
För att skriva tester i Elm, använd `elm-test`-paketet. Här är ett enkelt exempel:

```Elm
import Expect
import Test exposing (Test, describe, test)
import YourModule exposing (yourFunction)

suite : Test
suite =
    describe "YourModule"
        [ test "yourFunction returns the expected result" <|
            \_ -> yourFunction "input" |> Expect.equal "expected output"
        ]

-- För att köra testerna, i terminalen:
-- $ elm-test
```

Exempel på utdata:

```
TEST RUN FAILED

Duration: 3 ms
Passed:   0
Failed:   1

1) YourModule yourFunction returns the expected result
   Expected "expected output" but got "actual output"
```

## Djupdykning
Elm-test är skapat av elm-community och är standardvalet för att skriva tester i Elm. Det var inte en del av den ursprungliga Elm-versionen, men har blivit en central del av ekosystemet. Alternativ till `elm-test` är begränsade på grund av Elms arkitektur, men `elm-test` täcker de flesta användningsfall. Testerna körs på Node.js och kan integreras i CI/CD-pipeline för automatisering.

## Se Också
- [Elm Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
