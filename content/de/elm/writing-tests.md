---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, Code zu erstellen, der überprüft, ob andere Codeteile wie erwartet funktionieren. Programmierer machen das, um Fehler zu vermeiden, die Codequalität zu sichern und Refactorings zu erleichtern.

## How to:
Elm benutzt [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) für Tests. Installiere es und schreibe klare, verständliche Tests. Beispiele:

```Elm
import Expect
import Test exposing (..)
import ExampleProgram exposing (..)

suite : Test
suite =
  describe "Beispiel Tests"
    [ test "2 plus 2 ist 4" <|
        \_ -> Expect.equal 4 (2 + 2)
    , test "Leere Liste hat Länge 0" <|
        \_ -> Expect.equal 0 (List.length [])
    ]

-- Führe Tests mit `elm-test` aus.
-- Ergebnis sollte sein:
-- TEST RUN PASSED
-- 
-- Duration: 42 ms
-- Passed:   2
-- Failed:   0
```

## Deep Dive:
Tests in Elm haben eine kurze Geschichte, sind aber professionell geworden. `elm-test` bietet eine reine Elm Erfahrung, ähnlich wie Jest in JavaScript. Im Vergleich: andere Sprachen nutzen oft externe Tools, während Elm im Ökosystem bleibt. Die Implementierung setzt pure Funktionen und auf immutability basierende Strukturen ein, wodurch die Tests vorhersehbar und zuverlässig sind.

## See Also:
- Elm Test Dokumentation: [https://package.elm-lang.org/packages/elm-explorations/test/latest/](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- Beispielprojekte auf GitHub: Suche nach `elm-test` in Repositories für echte Anwendungsbeispiele.
