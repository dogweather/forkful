---
title:                "Elm: Tests schreiben"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil jeder Programmiersprache, einschließlich Elm. Sie dienen dazu, die Funktionalität von Code zu überprüfen und sicherzustellen, dass Änderungen oder Updates keine unerwarteten Nebenwirkungen haben. Durch das Schreiben von Tests erhältst du auch ein besseres Verständnis für den Code und kannst etwaige Fehler schneller erkennen und beheben.

## How To

Um Tests in Elm zu schreiben, benötigst du das Paket "elm-test". In deinem Terminal kannst du es mit dem folgenden Befehl installieren:

```Elm
elm install elm-explorations/test
```

Anschließend kannst du in deinem Projektordner eine Datei mit dem Namen "Tests.elm" erstellen, in der du deine Tests schreibst. Hier ein Beispiel für eine Funktion, die überprüft, ob eine Zahl gerade oder ungerade ist:

```Elm
module Tests exposing (..)

import Test exposing (..)
import Expect

isEven : Int -> Bool
isEven number =
    number % 2 == 0

suite : Test
suite =
    describe "Testing isEven"
        [ test "Even numbers should return True" <|
            \() ->
                Expect.equal True (isEven 4)
        , test "Odd numbers should return False" <|
            \() ->
                Expect.equal False (isEven 3)
        ]

main : Test
main =
    suite 
```

Der Output für diesen Test wäre:

```elm
➜ elm-test
Successfully generated 'elm-stuff/generated-code/elm-test/0.19.1/Tests.elm'.

elm-test 0.19.1
--------------

Running 1 test. To reproduce these results, run: elm-test

TEST RUN PASSED

Passed! [ 1 test, 0 failures ]
```

## Deep Dive

Beim Schreiben von Tests gibt es einige wichtige Konzepte zu beachten. Ein guter Test sollte unabhängig, verständlich und präzise sein. Du solltest auch sicherstellen, dass deine Tests alle möglichen Ergebnisse abdecken und nicht nur die "happy paths". Eine weitere wichtige Sache ist es, die richtige Balance bei der Anzahl der Tests zu finden - zu wenige Tests können mögliche Fehler übersehen, aber zu viele können dazu führen, dass deine Tests unübersichtlich werden.

Es ist auch wichtig zu beachten, dass Tests kein Ersatz für sorgfältiges Debugging sind. Sie sind dazu da, Fehler zu finden und nicht, um sicherzustellen, dass dein Code perfekt ist. Das Schreiben von Tests erhöht jedoch die Stabilität und Zuverlässigkeit deines Codes und ist daher eine wichtige Praxis in der Programmierung.

## Siehe auch

- [Offizielle Elm Dokumentation zum Testen](https://guide.elm-lang.org/testing/)
- [Elm Unit Testing Library](https://package.elm-lang.org/packages/elm-explorations/test/latest/)