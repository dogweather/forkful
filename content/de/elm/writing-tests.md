---
title:                "Elm: Test schreiben"
simple_title:         "Test schreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Wenn du noch nicht mit Elm programmiert hast, könnte dir das Schreiben von Tests vielleicht zunächst unnötig erscheinen. Aber Tests sind eine wichtige Methode, um sicherzustellen, dass dein Code richtig funktioniert und zuverlässig bleibt. Sie helfen auch dabei, Fehler schneller zu entdecken und zu beheben.

## So geht's

Zunächst musst du das `elm-test` Paket installieren, falls du es noch nicht hast. Dann kannst du Tests in einer separaten Datei schreiben oder sie direkt in deinem Hauptcode integrieren. Hier ein einfaches Beispiel:

```elm
import Expect exposing (equal)

add : Int -> Int -> Int
add x y =
  x + y

-- Testfunktion für die add-Funktion
testAdd : Test
testAdd =
  describe "Testing the add function"
    [ test "1 + 2 should equal 3" <|
        \_ -> Expect.equal 3 (add 1 2)
    ]

-- Aufruf der Tests
tests : Test
tests =
  describe "All tests"
    [ testAdd
    ]
```

Die `add` Funktion wird durch die `test` Funktion getestet, indem eine erwartete Ausgabe mit der tatsächlichen Ausgabe verglichen wird. Wenn alle Tests bestehen, erhältst du eine Erfolgsmeldung. Ansonsten wirst du über die fehlgeschlagenen Tests informiert und kannst entsprechend reagieren.

## Tiefergehende Informationen

Es gibt verschiedene Arten von Tests, die du schreiben kannst, wie z.B. Unit-Tests, Integrationstests oder End-to-End-Tests. Es kommt darauf an, welchen Teil deines Codes du testen möchtest und welches Ergebnis du erwartest. Es ist auch wichtig, zu verstehen, dass Tests kein Ersatz für sorgfältiges Programmieren sind – sie sollten zusätzlich dazu eingesetzt werden.

Du kannst auch mit dem `elm-verify-examples` Paket Beispielcode in deine Dokumentation integrieren und diesen automatisch testen lassen. Dadurch sicherst du dir, dass dein Beispielcode immer aktuell und funktionsfähig ist.

## Siehe auch

- Offizielle Dokumentation zu [Tests](https://guide.elm-lang.org/testing/)
- Tutorial zum Schreiben von [Tests in Elm](https://www.elm-tutorial.org/de/08-full-form-app/10-testing.html)
- Beispielprojekt mit [elm-test](https://github.com/laszlohordos/elm-test-example) und [elm-verify-examples](https://github.com/laszlohordos/elm-verify-examples-example)