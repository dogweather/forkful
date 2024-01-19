---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein temporäres Datei-Erzeugen ist der Prozess, eine Datei zu schaffen, die zur Speicherung temporärer Daten während einer Sitzung genutzt wird. Programmierer machen das, um sensiblen Daten-Verlust zu vermeiden und die Performance zu verbessern.

## So geht's:

Elm bietet keine direkten Möglichkeiten zur Dateierstellung. Es ist eine sprachliche aufgedeckt, die zur Verbesserung der Web-Front-Ends entwickelt wurde, und daher nicht direkt mit Dateisystemen interagieren kann.

Aber in der Welt von Elm, kannst du möglicherweise mit temporären Daten auf diese Weise umgehen mit hilfe des `localStorage`:

```Elm
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = String

main =
  Nav.program Url.Parser.identity
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
  ( "Daten laden...", Nav.load ("/it.com/"+ url.fragment) )

type Msg = GotUrl (Result Nav.LoadError String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotUrl (Ok url) ->
      (url, Cmd.none)

    GotUrl (Err _) ->
      ( "Fehler beim Laden der URL!", Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Nav.pushUrl model ("http://example.com/" ++ model)) ] [ text "Speichern" ]
        ]
```

## Weitergehende Information

In der Vergangenheit waren temporäre Dateien üblicherweise zur Lösung von Speicherproblemen verwendet. Heute sind sie immer noch ein praktikables Mittel zum Datenaustausch zwischen Systemen und Prozessen. Elm, als eher Frontend-orientierte Sprache, ist nicht direkt für solche Aufgaben ausgelegt.

Wenn du Alternativen suchst, könntest du eine Backend-Sprache wie Node.js in Betracht ziehen, die eine mächtige und flexible Bibliothek für Dateioperationen bietet. Aber in Elm, ist der Verwendung von `localStorage` eine probate Methode um temporäre Daten zu speichern.

## Siehe auch

Schau dir auch die Links an:
1. Temporäre Daten in Webanwendungen: [link]
2. Nutzung von `localStorage` in Elm: [link]
3. Node.js Datei-Operationen: [link]