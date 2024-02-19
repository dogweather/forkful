---
aliases:
- /de/elm/using-a-debugger/
date: 2024-01-26 03:48:50.858891-07:00
description: "Das Debuggen in Elm beinhaltet das Identifizieren und Entfernen von\
  \ Fehlern aus Ihrem Code. Programmierer machen das, um sicherzustellen, dass ihre\u2026"
lastmod: 2024-02-18 23:09:04.786332
model: gpt-4-0125-preview
summary: "Das Debuggen in Elm beinhaltet das Identifizieren und Entfernen von Fehlern\
  \ aus Ihrem Code. Programmierer machen das, um sicherzustellen, dass ihre\u2026"
title: Einsatz eines Debuggers
---

{{< edit_this_page >}}

## Was & Warum?
Das Debuggen in Elm beinhaltet das Identifizieren und Entfernen von Fehlern aus Ihrem Code. Programmierer machen das, um sicherzustellen, dass ihre Anwendungen korrekt funktionieren und um die Codequalität zu verbessern. Elm's starkes Typsystem fängt viele Probleme zur Compile-Zeit auf, aber Runtime-Debugging-Tools sind wesentlich, um logische Fehler und unerwartete Verhaltensweisen auszubügeln.

## Wie:
Elm hat keinen eingebauten Debugger im traditionellen Sinn, wie es beispielsweise JavaScript mit Browser-Entwicklertools tut. Jedoch hat die Elm-Community Tools entwickelt, um diese Lücke zu schließen. Hier ist, wie Sie `elm-debug-transformer` nutzen können, um Ihre Elm-App zu debuggen:

```Elm
-- Installieren Sie elm-debug-transformer (Node-Paket)

1. npm install -g elm-debug-transformer

-- Verwenden Sie elm-debug-transformer, um Ihre App zu starten

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Sobald `elm-debug-transformer` läuft, erstellt es eine WebSocket-Verbindung für das Logging. Sie werden Debug-Informationen in der Konsole Ihres Browsers sehen, wo Sie die Datenstrukturen Ihres Programms zu gegebenen Punkten in Ihrer Anwendung untersuchen können.

In Elm 0.19 und später können die Funktionen des `Debug`-Moduls wie `Debug.log` und `Debug.todo` Ihnen helfen, Werte zu verfolgen und bewusst unvollendete Teile Ihres Codes zu markieren. So nutzen Sie Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Erhöhen" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Verringern" { model | count = model.count - 1 }, Cmd.none )
```

Sie werden "Erhöhen" oder "Verringern" Nachrichten in der Konsole Ihres Browsers sehen, zusammen mit dem neuen Zustand des `model`.

## Tiefer Einblick
Der Autor von Elm, Evan Czaplicki, zielte darauf ab, eine Sprache zu erstellen, in der häufige Bugs unmöglich oder leicht zu fangen sind. Diese Philosophie ist der Grund, warum Elms Kern keine traditionellen Debugging-Funktionen enthält. Elms statische Analyse und Typeninferenz tragen massiv dazu bei, Laufzeitfehler zu reduzieren, was den Bedarf an ausgeklügeltem Runtime-Debugging verringert. Historische Alternativen umfassten die Nutzung des mittlerweile veralteten `elm-reactor`, der Time-Travel-Debugging bot – eine Möglichkeit, Aktionen in Ihrer App zurückzuspulen und erneut abzuspielen.

Heute helfen Tools wie `elm-debug-transformer` und die Verwendung des Elm-`Debug`-Moduls, die Lücke zu schließen. Während das `Debug`-Modul nur während der Entwicklung verwendet werden soll und vor Produktionsbuilds entfernt werden sollte, ist es ein unschätzbares Werkzeug zum Identifizieren und Protokollieren von Zustandsänderungen.

Beachten Sie, dass traditionelle JavaScript-Debugging-Techniken wie Breakpoints oder schrittweise Ausführung in Elm aufgrund seiner Architektur und der Verarbeitung von Zustandsupdates durch die Elm-Runtime nicht direkt anwendbar sind. Elm ermutigt Sie dazu, Ihr Programm so zu strukturieren, dass der Datenfluss klar ist und strikten Typen sowie Unveränderlichkeitsgarantien folgt, was die Fälle, in denen Debugging benötigt wird, minimiert.

## Siehe auch
- Elms offizieller Leitfaden zur Handhabung von Laufzeit-Ausnahmen: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub-Repository: https://github.com/kraklin/elm-debug-transformer
- Elm-Diskussionsfaden, der Debugging-Strategien erörtert: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elm-`Debug`-Modul Dokumentation: https://package.elm-lang.org/packages/elm/core/latest/Debug
