---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:18.221595-07:00
description: "Das Generieren von Zufallszahlen in Elm erfordert die Verwendung des\
  \ `Random`-Moduls, um Pseudozufallszahlen zu erzeugen, die f\xFCr eine Vielzahl\
  \ von\u2026"
lastmod: '2024-03-13T22:44:53.800651-06:00'
model: gpt-4-0125-preview
summary: "Das Generieren von Zufallszahlen in Elm erfordert die Verwendung des `Random`-Moduls,\
  \ um Pseudozufallszahlen zu erzeugen, die f\xFCr eine Vielzahl von\u2026"
title: Zufallszahlen generieren
weight: 12
---

## Was & Warum?
Das Generieren von Zufallszahlen in Elm erfordert die Verwendung des `Random`-Moduls, um Pseudozufallszahlen zu erzeugen, die für eine Vielzahl von Aufgaben wie Spiele, Simulationen und sogar als Teil von Algorithmen, die stochastische Prozesse benötigen, nützlich sind. Diese Fähigkeit ermöglicht es Entwicklern, Unvorhersehbarkeit und Vielfalt in ihre Anwendungen zu bringen, was die Benutzererfahrung und Funktionalität verbessert.

## Wie geht das:
Elms reine funktionale Natur bedeutet, dass du nicht direkt Zufallszahlen generieren kannst, wie du es vielleicht in imperativen Sprachen tun würdest. Stattdessen verwendest du das `Random`-Modul in Verbindung mit Befehlen. Hier ist ein einfaches Beispiel, das eine zufällige Ganzzahl zwischen 1 und 100 generiert.

Zuerst installiere das `Random`-Modul mit `elm install elm/random`. Dann importiere es in deine Elm-Datei, zusammen mit den notwendigen HTML- und Event-Modulen, so:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Um dies zu einem eigenständigen Beispiel zu machen, kannst du dieses Grundgerüst hinzufügen:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Als Nächstes definiere einen **Befehl**, um eine Zufallszahl zu generieren. Dies beinhaltet das Einrichten eines `Msg`-Typs, um die Zufallszahl zu verarbeiten, sobald sie generiert wurde, ein `Model`, um sie zu speichern, und eine Update-Funktion, um alles zusammenzubinden.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Um eine Zahlengenerierung auszulösen, würdest du eine `Generate`-Nachricht senden, zum Beispiel durch einen Knopf in deiner Ansicht:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Zufallszahl: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generieren" ]
        ]
```

Wenn du auf den "Generieren"-Knopf klickst, wird eine zufällige Zahl zwischen 1 und 100 angezeigt.

Dieser einfache Ansatz kann angepasst und erweitert werden, indem andere Funktionen im `Random`-Modul genutzt werden, um Zufallsfloats, Listen oder sogar komplexe Datenstrukturen basierend auf benutzerdefinierten Typen zu erzeugen, und bietet so einen riesigen Spielplatz für die Hinzufügung von Unvorhersehbarkeit zu deinen Elm-Anwendungen.

Der Elm-Leitfaden geht viel detaillierter darauf ein. Es hat auch [ein Beispiel für das Würfeln eines sechsseitigen Würfels](https://guide.elm-lang.org/effects/random).
