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

## Warum 

Warum sollte man sich die Mühe machen, Tests in seinem Elm Code zu schreiben? Nun, Tests ermöglichen es uns, unsere Codebase auf Fehler zu überprüfen, bevor wir ihn ausführen. Es gibt uns auch die Möglichkeit, Änderungen in unserem Code zu verfolgen und sicherzustellen, dass sie nicht unerwartet andere Funktionen beeinflussen.

## Wie geht's?

Das Schreiben von Tests in Elm ist einfacher als Sie vielleicht denken. Zuerst müssen Sie die Bibliothek elm-test installieren, indem Sie das folgende Kommando in Ihrem Terminal ausführen:

```
elm install elm-community/elm-test
```

Als nächstes müssen Sie eine `tests` Map in Ihrer `elm.json` Datei hinzufügen, die auf Ihre Testmodule verweist. Zum Beispiel:

```
{
    "test-dependencies": {
        "direct": {
            "elm-explorations/test": "1.2.2",
            "elm-community/elm-test": "4.2.1"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.1.0"
        }
    }, 
    "tests": {
        "front-end": {
            "source-directories": [
                "tests"
            ],
            "exposed-modules": [
                "Tests.FrontEnd"
            ]
        }
    }
}
```

Als nächstes müssen Sie ein Testmodul erstellen, das Ihre Funktionen testet. Hier ist ein Beispiel dafür, wie Sie eine einfache Funktion testen können, die eine Zahl verdoppelt:

```
module Tests.FrontEnd exposing (suite)

import Test exposing (..)
import Expect exposing (expect)
import Main exposing (double)

suite : Test
suite =
    describe "Double function"
        [ test "doubles positive numbers" <|
            \() ->
                expect (double 2) toBe 4
        , test "doubles negative numbers" <|
            \() ->
                expect (double (-2)) toBe (-4)
        ]
```

Und hier ist die Ausgabe, die Sie erwarten würden, wenn Sie diesen Test erfolgreich ausführen:

```
TEST RUN PASSED

Passed: 2
Failed: 0
Todo: 0
SKIPPED: 0
```

## Tiefere Einblicke

Jetzt wissen Sie, wie Sie Tests in Elm schreiben und ausführen können, aber es gibt noch einige Tipps, die Ihnen dabei helfen können, effektive Tests zu schreiben:

- Versuchen Sie, jeden Zweig Ihrer Funktionen zu testen und auch den ungewöhnlichen Eingabefällen Aufmerksamkeit zu schenken.
- Nutzen Sie die `Debug.log` Funktion, um sich die Werte Ihrer Variablen während des Testens ausgeben zu lassen, um Ihnen bei der Fehlersuche zu helfen.
- Verwenden Sie `expectationHelp` und `((|>)` um Ihre Erwartungen besser zu formulieren und Ihren Code lesbarer zu machen.

Es gibt viele verschiedene Möglichkeiten, Ihre Tests zu schreiben, und jeder hat seine eigene Herangehensweise. Finden Sie heraus, was für Sie am besten funktioniert, und versuchen Sie, konsistente Tests zu schreiben, die Ihre Codeabdeckung verbessern.

## Siehe auch

- Offizielle Elm Test Dokumentation: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Ein Einführung in das Testen in Elm: https://dev.to/richardapearson/testing-in-elm-4j7p
- TDD in Elm: https://gist.github.com/rtfeldman/2509ed34b008a0bf98f9