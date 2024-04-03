---
date: 2024-01-19
description: "How to: (Jak to zrobi\u0107:) Elm nie ma wbudowanego wsparcia dla YAML,\
  \ wi\u0119c trzeba u\u017Cy\u0107 zewn\u0119trznej biblioteki lub konwertowa\u0107\
  \ YAML do JSON. Oto przyk\u0142ad\u2026"
lastmod: '2024-03-13T22:44:35.342159-06:00'
model: unknown
summary: "Elm nie ma wbudowanego wsparcia dla YAML, wi\u0119c trzeba u\u017Cy\u0107\
  \ zewn\u0119trznej biblioteki lub konwertowa\u0107 YAML do JSON."
title: Praca z yaml
weight: 41
---

## How to: (Jak to zrobić:)
Elm nie ma wbudowanego wsparcia dla YAML, więc trzeba użyć zewnętrznej biblioteki lub konwertować YAML do JSON. Oto przykład konwersji YAML do JSON przy użyciu JavaScript i wysyłki danych do Elm:
```Elm
port module Main exposing (..)

-- Importujemy Ports i Json.Decode
import Ports
import Json.Decode as Decode

-- Definicja typu dla danych z konfiguracji
type alias Config =
    { port : Int
    , host : String
    }

-- Dekoder JSON dla Config
configDecoder : Decode.Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "port" Decode.int)
        (Decode.field "host" Decode.string)

-- Nasłuchiwanie danych z YAML przesłanych przez port
port configFromYaml : (Config -> msg) -> Sub msg

-- Subskrybujemy port w `main`
main : Program () Config Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : (Config, Cmd Msg)
init =
    (Config 0 "", Cmd.none)

type Msg
    = NewConfig Config

update : Msg -> Config -> (Config, Cmd Msg)
update msg model =
    case msg of
        NewConfig newConfig ->
            (newConfig, Cmd.none)

subscriptions : Config -> Sub Msg
subscriptions _ =
    configFromYaml NewConfig
```
W JavaScript:
```JavaScript
// Przykład konwersji YAML do JSON i wysłania do Elm poprzez port
var jsYaml = require('js-yaml');
var fs = require('fs');

// Załóżmy, że mamy plik config.yaml
var yamlText = fs.readFileSync('config.yaml', 'utf8');
var config = jsYaml.safeLoad(yamlText);

// Załóżmy, że mamy zdefiniowany port 'configFromYaml' w Elm
elmApp.ports.configFromYaml.send(config);
```
Ważne: Musisz zintegrować Elm z JavaScript, aby obsłużyć YAML.

## Deep Dive (Dogłębna analiza)
YAML powstał w 2001 roku jako human-friendly alternatywa do XML i JSON. Inne języki mają biblioteki do bezpośredniej pracy z YAML (np. PyYAML dla Pythona, ruamel.yaml dla Ruby), ale w Elm musisz to robić przez interfejs portów i JavaScript. Implementacja może się różnić w zależności od potrzeb, ale zaletą jest łatwość czytania i pisanie YAML.

## See Also (Zobacz również)
- YAML Specyfikacja: https://yaml.org/spec/1.2/spec.html
- Biblioteka `js-yaml` do konwersji w JavaScript: https://github.com/nodeca/js-yaml
- Elm Ports: https://guide.elm-lang.org/interop/ports.html
- Przykład konwersji YAML do JSON: https://stackoverflow.com/questions/49911616/converting-yaml-to-json-using-javascript
