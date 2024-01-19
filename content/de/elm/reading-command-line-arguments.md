---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Command Line Arguments (Kommandozeilenargumente) sind Informationen, die einem Programm beim Start übergeben werden. Sie sind besonders hilfreich, wenn wir den Ablauf eines Programms basierend auf diesen Eingaben steuern wollen.

## Wie geht das:

Elm erlaubt es uns nicht direkt, Kommandozeilenargumente zu lesen. Um diese Funktionalität zu erreichen, müssen wir eine JavaScript-Brücke verwenden, die als Port bezeichnet wird. Unten ist ein einfaches Beispiel dafür:

```Elm
port module Main exposing (..)

port toJavascript : String -> Cmd msg
port fromJavascript : (String -> msg) -> Sub msg

type Msg = NewMessage String 

main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \_ -> Html.none
        }

init : String -> ( (), Cmd Msg )
init flags =
    ( (), fromJavascript NewMessage )

update : Msg -> model -> ( model, Cmd Msg )
update msg model =
    case msg of
        NewMessage newMessage ->
            ...
```

## Vertiefung:

Elm nutzt eine reine, funktionale Sprache und erlaubt keine Seiteneffekte - deshalb kann es nicht direkt auf die Kommandozeile zugreifen. Stattdessen verwenden wir Ports, um mit JavaScript zu kommunizieren. Historisch gesehen sind Kommandozeilenargumente seit den frühen Tagen der Programmierung vorhanden, sie bieten eine einfache Art, die Ausführung eines Programms zu steuern.

Ein alternativer Ansatz wäre, die benötigten Daten irgendwie in die Umgebung zu laden, bevor Elm startet. Denn was Elm gut kann, ist der Umgang mit User-Inputs in einer Web-Umgebung.

Es ist wichtig zu verstehen, dass Ports nicht in `elm reactor` funktionieren. Sie erfordern eine HTML-Hülle, die das Elm-Programm hält. Das macht sie für Projekte geeignet, die bereits eine gemischte Codebasis haben oder wo eine reine Elm-Lösung nicht ausreicht.

## Siehe auch:

- Elm Ports Dokumentation: https://guide.elm-lang.org/interop/ports.html
- Elm Command Line Arguments Diskussion auf Discourse: https://discourse.elm-lang.org/t/command-line-arguments/6387
- Elm-Programm mit Flags: https://package.elm-lang.org/packages/elm/browser/latest/Browser-Application#programWithFlags 

Es gibt keine "Fazit"-Sektion in diesem Artikel. Ihr solltet nun eine Vorstellung davon haben, wie man Kommandozeilenargumente in Elm handhabt.