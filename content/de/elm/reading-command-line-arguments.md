---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Elm: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen wir es?
Das Lesen von Befehlszeilenargumenten ist ein wichtiger Teil des Programmierens. Es ermöglicht uns, Informationen von der Kommandozeile zu erhalten und sie in unserem Code zu verwenden. Dadurch können wir interaktive Programme erstellen, die auf Benutzereingaben reagieren. 

## Wie funktioniert es?
In Elm können wir die Befehlszeilenargumente mit Hilfe der Funktion `Elm.Platform.worker` lesen. Diese Funktion erwartet eine Nachricht und gibt uns eine `Cmd` zurück. Wir können die Befehlszeilenargumente dann in unserer Nachricht verarbeiten und verwenden. Hier ist ein Beispielcode:

```
Elm.Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }
```

Dieser Code liest die Befehlszeilenargumente und ruft dann die Funktion `update` auf, um die Nachricht zu verarbeiten.

Das folgende Beispiel zeigt, wie wir die Befehlszeilenargumente in unserer `init` Funktion verarbeiten können:

```
init : () -> ( Model, Cmd Msg )
init _ =
    ( Model, Elm.Cmd.none )
```

In diesem Beispiel haben wir eine leere Nachricht an die Funktion übergeben (`_`), da wir die Befehlszeilenargumente nicht verwenden wollen. Die Funktion `init` gibt dann den gewünschten Zustand `Model` und eine leere `Cmd` zurück.

## Tiefergehende Einblicke
Das Lesen von Befehlszeilenargumenten ist eine sehr nützliche Fähigkeit in der Programmierung. Es gibt jedoch auch alternative Methoden, wie z.B. die Verwendung von Umgebungsvariablen oder direkter Benutzereingaben. 

Die Funktion `Elm.Platform.worker` wurde als Teil des Elm-Debugger-Architektur eingeführt, um die Debugging-Fähigkeiten von Elm zu verbessern. Sie wird auch von anderen Elm-Paketen verwendet, wie zum Beispiel dem Paket `elm-explorations/benchmark`.

## Siehe auch
- [Elm-Dokumentation zu Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
- [Wie man Befehlszeilenargumente in C++ liest](https://stackoverflow.com/questions/3024197/how-do-i-read-command-line-arguments-in-c)
- [Alternative Methoden zum Lesen von Befehlszeilenargumenten in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#getProperties--)