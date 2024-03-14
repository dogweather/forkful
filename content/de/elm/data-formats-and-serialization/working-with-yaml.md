---
date: 2024-01-19
description: "YAML ist ein datenorientiertes Format zum Speichern und \xDCbertragen\
  \ von Informationen, \xE4hnlich wie JSON, aber menschenlesbarer. Programmierer verwenden\
  \ es\u2026"
lastmod: '2024-03-13T22:44:53.825912-06:00'
model: unknown
summary: "YAML ist ein datenorientiertes Format zum Speichern und \xDCbertragen von\
  \ Informationen, \xE4hnlich wie JSON, aber menschenlesbarer. Programmierer verwenden\
  \ es\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein datenorientiertes Format zum Speichern und Übertragen von Informationen, ähnlich wie JSON, aber menschenlesbarer. Programmierer verwenden es für Konfigurationen, Datenimport/-export und weil es einfach zu lesen und zu schreiben ist.

## Anleitung:
Elm hat standardmäßig keine eingebaute Bibliothek zur YAML-Analyse. Du musst eine externe JavaScript-Bibliothek verwenden und Elm's Ports nutzen, um YAML-String zu konvertieren.

```Elm
port module Main exposing (..)

-- Definiere Ports zum Senden und Empfangen von YAML
port yamlToElm : (String -> msg) -> Sub msg
port elmToYaml : String -> Cmd msg

type alias Model = 
    { content : String }

type Msg
    = ReceiveYaml String
    | SendYaml

-- Initialisiere dein Model
init : Model
init =
    { content = "" }

-- Update-Funktion verarbeitet empfangene YAML-Daten
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveYaml yamlString ->
            ({ model | content = yamlString }, Cmd.none)

        SendYaml ->
            (model, elmToYaml model.content)

-- Abonniere die Ports
subscriptions : Model -> Sub Msg
subscriptions model =
    yamlToElm ReceiveYaml

-- Main Funktion mit init, update und subscriptions
main : Program () Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
```

Im zugehörigen JavaScript:

```javascript
// Abonniere den Port aus Elm
app.ports.elmToYaml.subscribe(function(yamlString) {
  // Verwende eine YAML-JS-Bibliothek zum Parsen
  var data = YAML.parse(yamlString);
  app.ports.yamlToElm.send(data);
});
```

## Tiefgang:
YAML (YAML Ain't Markup Language) wurde in den frühen 2000ern entwickelt. Die Lesbarkeit und die Einfachkeit stehen im Vordergrund. Alternative Formate wie JSON oder XML sind automatisierter zu parsen, bieten jedoch weniger Mensch-Freundlichkeit. Um YAML in Elm zu verarbeiten, benötigst du JavaScript-Interoperabilität, da Elm eine reine Sprache bleibt und keine arbiträren Seiteneffekte ermöglicht.

## Siehe auch:
* YAML-Webseite: [https://yaml.org](https://yaml.org)
* JavaScript-YAML-Parser: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
* Elm Guide zu Ports: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
