---
title:                "Arbeiten mit JSON"
html_title:           "Elm: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man sich mit JSON in Elm beschäftigen könnte. Zum einen ist es ein häufig verwendetes Datenformat für den Austausch von Informationen in modernen Anwendungen. Darüber hinaus bietet Elm eine sehr einfache und saubere Möglichkeit, mit JSON-Daten umzugehen, was die Entwicklung von dynamischen und flexiblen Anwendungen erleichtert.

## So geht's

Um mit JSON in Elm zu arbeiten, benötigen wir das [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/) Modul. Wir können es importieren, indem wir folgende Zeile in unserer Elm-Datei hinzufügen:

```
import Json.Decode exposing (..)
```

Als nächstes müssen wir eine Decoder-Funktion definieren, um JSON-Objekte in Elm zu parsen. Ein Decoder ist im Grunde eine Funktion, die ein JSON-Objekt als Eingabe erhält und ein Elm-Datentyp als Ausgabe zurückgibt. Zum Beispiel können wir eine einfache JSON-Datenstruktur wie folgt definieren:

```
type alias User =
    { name : String
    , age : Int
    , isAdmin : Bool
    }
```

Um diese Struktur aus einem JSON-Objekt zu parsen, können wir folgenden Decoder erstellen:

```
userDecoder: Decoder User
userDecoder =
    map3 User
        (field "name" string)
        (field "age" int)
        (field "isAdmin" bool)
```

In diesem Beispiel verwenden wir die `map3` Funktion, um die Ausgabe des `User` Datentyps zu erstellen, indem wir die Werte aus den Feldern des JSON-Objekts extrahieren.

Nachdem wir unseren Decoder erstellt haben, können wir nun die `decodeValue` Funktion verwenden, um unser JSON-Objekt zu parsen. Zum Beispiel:

```
jsonString = """
    {
        "name": "Lisa",
        "age": 25,
        "isAdmin": true
    }
    """

result = decodeValue userDecoder (Json.Decode.fromString jsonString)
```

Der `result` wird nun ein `Ok User` oder ein `Err String` sein, je nachdem, ob das Parsen erfolgreich war oder nicht.

## Tiefer gehen

Das Erlernen von weiteren Funktionen und Erstellung von komplexeren Decodern ermöglicht es uns, tiefer in die Arbeit mit JSON in Elm einzutauchen. Wir können zum Beispiel auch verschachterte JSON-Strukturen mithilfe von `index` und `field` Funktionen parsen, Arrays mit `Array` Funktionen behandeln oder sogar benutzerdefinierte Typen mit einem kombinierten Decoder erstellen.

Es gibt auch fortgeschrittene Konzepte wie `Json.Encode`, das uns ermöglicht, Elm-Daten in JSON zu serialisieren, und `Json.Encode.Pipeline`, das uns eine sauberere Syntax für die Definition von JSON-Encodern bietet.

Die Verwendung von Decodern in Verbindung mit HTTP-Anfragen ermöglicht es uns auch, dynamische Daten von APIs in unsere Elm-Anwendungen zu integrieren.

## Siehe auch

- [Elm-Dokumentation über JSON](https://guide.elm-lang.org/effects/json.html)
- [Tutorial: Parsing JSON in Elm](https://medium.com/@DarrenWeston/elm-tutorial-parsing-json-c749dec0de5f)
- [JSON im Elm-Programmierhandbuch](https://elmprogramming.com/json-in-elm.html)