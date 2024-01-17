---
title:                "Herunterladen einer Webseite"
html_title:           "Elm: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was ist es und warum?

Das Herunterladen einer Webseite ist ein Prozess, bei dem Computerprogramme den Inhalt einer Webseite von einem Server im Internet abrufen und anzeigen. Programmierer nutzen dieses Verfahren, um Daten von einer Webseite zu erhalten und sie in ihrer eigenen Anwendung zu verwenden.

## Wie geht's?

```Elm
port module Main exposing (..)

import Html exposing (..)
import Http

-- Definition der URL
url : String
url = "https://www.example.com"

type Msg
    = Received (Http.Result Http.Error String)

-- Ausführen des Requests
request : Cmd Msg
request =
    Http.getString url
        |> Task.attempt Received

-- Aufrufen der Funktion "request" und Anzeigen des Ergebnisses in der HTML-Seite
main : Program Never Model Msg
main =
    Html.program
        { init = ((), request)
        , view = (\_ -> div [] [ text "Webseite wurde heruntergeladen!" ] )
        , update = (\msg _ -> case msg of (Received result) -> ((), Cmd.none))
        , subscriptions = (\_ -> Sub.none)
        }
```

## Tief eintauchen

Das Herunterladen von Webseiten ist ein wesentlicher Bestandteil des Internets. Früher wurde dies hauptsächlich mit der Programmiersprache Python durchgeführt, aber mit der Entwicklung von Frameworks wie Elm ist es nun auch möglich, dies in einer funktionalen Sprache zu tun.

## Siehe auch

- Offizielle Dokumentation von Elm: https://guide.elm-lang.org/
- Eine detailliertere Erklärung zum Herunterladen von Webseiten mit Elm von CodeWall: https://www.codewall.co.uk/beginners-guide-to-downloading-web-pages-in-elm/
- Alternativen zum Herunterladen von Webseiten in anderen Programmiersprachen wie Python oder JavaScript