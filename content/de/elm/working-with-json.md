---
title:                "Arbeiten mit json"
html_title:           "Elm: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Arbeiten mit JSON ist ein häufiges Thema in der Programmierung. Es ermöglicht den Datenaustausch zwischen verschiedenen Anwendungen und Plattformen. Programmierer nutzen JSON, um strukturierte Daten zu speichern und zu übertragen, was es zu einem wichtigen Werkzeug in der heutigen Softwareentwicklung macht.

## Wie geht das?
Um mit JSON in Elm zu arbeiten, müssen wir zunächst das Paket "elm/json" importieren. Dann können wir strukturierte Daten in JSON-Format konvertieren und vice versa. Hier ist ein Beispiel:
```
import Json.Encode exposing (int, object, string, list)
import Json.Decode exposing (decodeValue, field)

myData =
    object
        [ ("name", string "Max Mustermann")
        , ("age", int 30)
        , ("hobbies", list [string "Programming", string "Reading"])
        ]

encodedData = 
    encode 4 myData

decodedData = 
    decodeValue 
        [ ("name", field "name" string)
        , ("age", field "age" int)
        , ("hobbies", field "hobbies" (list string))
        ] 
        encodedData

```
Das Ergebnis der Ausgabe ist ein konvertiertes JSON-Objekt.

## Tiefer tauchen
JSON (JavaScript Object Notation) wurde ursprünglich von Douglas Crockford entwickelt und ist ein gebräuchliches Format für den Datenaustausch. Obwohl es in erster Linie mit JavaScript assoziiert wird, unterstützen viele Programmiersprachen, einschließlich Elm, JSON. Alternativen zu JSON sind XML und YAML, aber JSON ist aufgrund seiner Lesbarkeit und einfacher Syntax beliebter. Elm bietet auch verschiedene Funktionen, um die Arbeit mit JSON zu vereinfachen, wie zum Beispiel die Möglichkeit, benutzerdefinierte Decoder und Encoder für komplexe Datenstrukturen zu erstellen.

## Sieh es dir an
Für weitere Informationen über die Arbeit mit JSON in Elm, schaue dir die offizielle Dokumentation an: https://package.elm-lang.org/packages/elm/json/latest/. Du kannst auch das "elm/json"-Paket auf GitHub finden: https://github.com/elm/json.