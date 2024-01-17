---
title:                "Tests schreiben"
html_title:           "Elm: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests sind Code-Stücke, die speziell dafür geschrieben werden, um zu überprüfen, ob andere Code-Stücke richtig funktionieren. Programmierer schreiben Tests, um sicherzustellen, dass ihr Code fehlerfrei ist und wie erwartet funktioniert. Dies kann Zeit und Mühe sparen, indem potenzielle Fehler frühzeitig erkannt und behoben werden können.

## Wie?

Elm bietet eine integrierte Test-Bibliothek namens "elm-test", die es Programmierern ermöglicht, leicht verständliche und gut strukturierte Tests zu schreiben. Hier ist ein Beispiel für einen einfachen Test:

```
import Expect exposing (equal)

onePlusOneEqualsTwo : Test 
onePlusOneEqualsTwo =
  Test.test "1 + 1 sollte 2 ergeben" 
    (\_ -> 
        Expect.equal 2 (1 + 1))

```

Dieser Test prüft, ob die Berechnung "1 + 1" tatsächlich den Wert 2 ergibt. Die Funktion "expect" wird verwendet, um zu überprüfen, ob das Ergebnis der Berechnung tatsächlich dem erwarteten Wert entspricht.

Ein weiteres Beispiel für einen Test mit einer Fehlermeldung:

```
import Expect exposing (equal)

panel = { width = 100, height = 200 }

widthShouldBe100 : Test
widthShouldBe100 =
  Test.test "Die Breite sollte 100 betragen"
    (\_ ->
        Expect.equal 100 (panel.width))

heightShouldBe200 : Test 
heightShouldBe200 =
  Test.test "Die Höhe sollte 200 betragen"
    (\_ ->
        Expect.equal 200 (panel.height))
```

Hier wird überprüft, ob das Panel die erwarteten Werte für Breite und Höhe enthält. Wenn das Panel jedoch versehentlich falsch initialisiert wurde und die Werte nicht den Erwartungen entsprechen, wird eine Fehlermeldung ausgegeben, die dem Programmierer hilft, den Fehler zu finden und zu beheben.

## Tiefer Einblick

Tests haben in der Programmierung eine lange Geschichte. Sie wurden ursprünglich entwickelt, um sicherzustellen, dass der Code richtig und zuverlässig funktioniert. Alternativen zu Tests sind manuelle Überprüfungen oder Debugger, aber diese können zeitaufwendig und fehleranfällig sein. Mit Tests können Programmierer effizient und effektiv sicherstellen, dass ihr Code fehlerfrei ist.

In Elm werden Tests durch die Funktion "test" innerhalb der Test-Module definiert. Diese Funktion akzeptiert zwei Argumente: einen String als Beschreibung des Tests und eine Funktion, die den tatsächlichen Test ausführt. Die Funktion "expect" wird verwendet, um das erwartete Ergebnis zu überprüfen.

## Siehe auch

- Die offizielle Dokumentation zu Tests in Elm: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Eine Einführung in Test-driven Development (TDD) mit Elm: https://medium.com/@thejameskyle/tdd-in-elm-4c859bde8a3b
- Eine Vergleich von Tests in Elm und in anderen Programmiersprachen: https://www.lambdacurry.dev/blog/testing-in-elm/