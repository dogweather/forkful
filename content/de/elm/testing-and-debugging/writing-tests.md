---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:22.308619-07:00
description: "Tests in Elm zu schreiben, bedeutet, Testf\xE4lle zu erstellen, um die\
  \ Korrektheit Ihres Elm-Codes zu \xFCberpr\xFCfen und sicherzustellen, dass er sich\
  \ wie\u2026"
lastmod: '2024-03-13T22:44:53.808526-06:00'
model: gpt-4-0125-preview
summary: "Tests in Elm zu schreiben, bedeutet, Testf\xE4lle zu erstellen, um die Korrektheit\
  \ Ihres Elm-Codes zu \xFCberpr\xFCfen und sicherzustellen, dass er sich wie\u2026"
title: Tests Schreiben
weight: 36
---

## Was & Warum?

Tests in Elm zu schreiben, bedeutet, Testfälle zu erstellen, um die Korrektheit Ihres Elm-Codes zu überprüfen und sicherzustellen, dass er sich wie erwartet verhält. Programmierer machen das, um frühzeitig Fehler zu finden, die Wartung zu erleichtern und die Qualität und Zuverlässigkeit ihrer Anwendungen zu verbessern.

## Wie geht das:

Elm verwendet das Paket `elm-explorations/test` für das Schreiben von Einheiten- und Fuzz-Tests. Beginnen Sie damit, das Paket Ihrem Projekt hinzuzufügen:

```elm
elm install elm-explorations/test
```

Erstellen Sie eine Testdatei, sagen wir `tests/ExampleTest.elm`, und importieren Sie die Testmodule. Hier ist ein einfacher Test, der eine Funktion `add : Int -> Int -> Int` überprüft:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Eine einfache Additionsfunktion"
        [ test "2 und 3 addieren ergibt 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Um Ihre Tests auszuführen, benötigen Sie `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Dies wird Ihre Tests kompilieren und die Ergebnisse in Ihrem Terminal ausgeben. Für das obige Beispiel sollte die Ausgabe etwa folgendes sein:

```
TESTLAUF BESTANDEN

Dauer: 42 ms
Bestanden: 1
Fehlgeschlagen: 0
```

Für ein komplexeres Beispiel, nehmen wir an, Sie möchten die Funktion `add` mit Fuzz-Tests prüfen, um sicherzustellen, dass sie eine breite Palette von ganzzahligen Eingaben korrekt verarbeitet. Sie würden Ihre `ExampleTest.elm` wie folgt ändern:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Add testen mit Fuzzing"
        [ fuzz int "Fuzz-Test für add mit zufälligen ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Führen Sie `elm-test` erneut aus, um die Fuzz-Tests in Aktion zu sehen. Die Ausgabe variiert bei zufälliger Eingabe, aber erfolgreiche Tests zeigen keine Fehler an:

```
TESTLAUF BESTANDEN

Dauer: 183 ms
Bestanden: 100
Fehlgeschlagen: 0
``` 

Diese Beispiele zeigen, wie man einfache Einheiten- und Fuzz-Tests in Elm schreibt und ausführt, indem man das Paket `elm-explorations/test` verwendet. Das Testen ist ein wesentlicher Teil des Entwicklungsprozesses und hilft dabei, sicherzustellen, dass Ihre Elm-Anwendungen zuverlässig sind und eine hohe Qualität aufweisen.
