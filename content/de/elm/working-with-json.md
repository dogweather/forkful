---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-json.md"
---

{{< edit_this_page >}}

# JSON in Elm: Klar und Einfach

Elm ist eine elegante Sprache für Web-Frontends, die besonderen Wert auf Benutzerfreundlichkeit und eine solide Architektur legt. Um Daten auszutauschen – speziell im Webkontext – kommt häufig JSON zum Einsatz, ein leichtgewichtiger Datenaustauschformat, der für Mensch und Maschine einfach zu lesen und zu schreiben ist. Elm bietet eine robuste und sichere Möglichkeit, mit JSON zu arbeiten, die wir uns hier genauer anschauen werden.

## Was & Warum?
JSON (JavaScript Object Notation) ist ein Format für den Datenaustausch. Es wird verwendet, weil es leichtgewichtig, menschenlesbar und maschinenverarbeitbar ist. In Webanwendungen nutzen wir JSON, um Daten von einem Server zu beziehen oder an einen Server zu senden.

## How to:
Elm verwendet das `Json.Decode` und `Json.Encode` Module, um mit JSON zu arbeiten. Hier sind einfache Beispiele, wie man JSON dekodiert und kodiert.

Dekodierung eines einfachen Objekts:

```Elm
import Json.Decode exposing (Decoder, string, int, field)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "name" string)
        (field "age" int)

-- Sample JSON
jsonString : String
jsonString =
    """
    { "name": "Max", "age": 25 }
    """

-- Benutze die Dekodierung
decodedUser : Result String User
decodedUser =
    Json.Decode.decodeString userDecoder jsonString
```

Kodierung eines einfachen Objekts:

```Elm
import Json.Encode exposing (object, string, int)

type alias User =
    { name : String
    , age : Int
    }

userEncoder : User -> Json.Encode.Value
userEncoder user =
    object
        [ ("name", string user.name)
        , ("age", int user.age)
        ]

-- Beispiel Nutzer
max : User
max =
    { name = "Max"
    , age = 25
    }

-- Benutze die Kodierung
encodedJson : String
encodedJson =
    Json.Encode.encode 0 (userEncoder max)
```

## Deep Dive
JSON ist seit 2001 ein Standard und wird in fast allen Programmiersprachen unterstützt. In Elm wird viel Wert auf Fehlervermeidung gelegt, daher müssen Decoder explizit definiert werden, damit die Datenstrukturen bekannt sind. Das macht den Code sicherer und vorhersehbarer.

Alternativ zu `Json.Decode` und `Json.Encode` gibt es auch Bibliotheken wie `elm-json-decode-pipeline`, die das Arbeiten mit JSON bequemer machen können, insbesondere bei komplexeren Datenstrukturen.

Ein kritischer Punkt bei der Arbeit mit JSON in Elm ist, dass Typ-Fehler zur Compile-Zeit aufgedeckt werden – das erhöht die Zuverlässigkeit von Elm-Applikationen.

## See Also
- Elm JSON Decode Dokumentation: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
- Elm JSON Encode Dokumentation: https://package.elm-lang.org/packages/elm/json/latest/Json-Encode
- Ein praktischer Guide zu `elm-json-decode-pipeline`: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/
