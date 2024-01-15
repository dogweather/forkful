---
title:                "Arbeiten mit yaml"
html_title:           "Elm: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn du eine einfache Methode suchst, um Daten in deinen Projekten zu strukturieren, dann könnte YAML die Lösung für dich sein. YAML ist eine Markup-Sprache, die auf lesbarem Text basiert und leicht zu verstehen und zu schreiben ist. Es wird oft für Konfigurationsdateien oder zum Speichern von Daten verwendet und ist in vielen Programmiersprachen wie Elm verfügbar.

## So geht's

Um YAML in Elm zu verwenden, müssen wir das Paket `elm-exploration/yaml` installieren. Dann können wir die `decode`-Funktion verwenden, um YAML in ein `Value` umzuwandeln.

```Elm
import Json.Decode exposing (Value)
import Yaml.Decode as Yaml

typealias MyData =
  { name : String
  , age : Int
  }

--YAML Daten
yamlData : String
yamlData = """
name: John
age: 30
"""

-- Yaml in Value umwandeln
value : Result Yaml.Error Value
value = Yaml.decode yamlData

-- Value in MyData umwandeln
myData : Result Yaml.Error MyData
myData =
  value
    |> Result.andThen (Yaml.decodeValue Yaml.string)
    |> Result.andThen (Yaml.decodeValue Yaml.int)
    |> Result.map2 MyData
        (\name age ->
          { name = name
          , age = age
          }
        )

```

### Ausgabe

```Elm
Ok { name = "John", age = 30 }
```

Mit der `decode`-Funktion können wir auch komplexere Datenstrukturen wie Listen oder verschachtelte Objekte decodieren. Hier ist ein Beispiel:

```Elm
import Json.Decode exposing (Value)
import Yaml.Decode as Yaml

typealias Fruit =
  { name : String
  , color : String
  }

-- YAML Daten
yamlData : String
yamlData = """
fruits:
  - name: Apple
    color: Red
  - name: Banana
    color: Yellow
"""

-- Yaml in Value umwandeln
value : Result Yaml.Error Value
value = Yaml.decode yamlData

-- Value in Liste von Fruit umwandeln
fruitList : Result Yaml.Error (List Fruit)
fruitList =
  value
    |> Result.andThen (Yaml.decodeField "fruits")
    |> Result.andThen (Yaml.decodeList decodeFruit)

-- Fruit decodieren
decodeFruit : Yaml.Decoder Fruit
decodeFruit =
  Yaml.map2 Fruit
    (Yaml.field "name" Yaml.string)
    (Yaml.field "color" Yaml.string)

```

### Ausgabe

```Elm
Ok [ { name = "Apple", color = "Red" }, { name = "Banana", color = "Yellow" } ]
```

## Tiefergehende Informationen

Beim Arbeiten mit YAML in Elm gibt es ein paar Dinge zu beachten:

- YAML unterstützt keine Kommentare, daher müssen sie vorher manuell entfernt werden.
- YAML unterstützt nur Zeilenumbrüche als Trennzeichen, weshalb `elm-exploration/yaml` auch keine Zeilenumbrüche innerhalb von Werten zulässt. Wenn du also einen Zeilenumbruch benötigst, musst du ihn als `\n` in deinem YAML-Code angeben.
- Bei der Decodierung von verschachtelten Objekten musst du sicherstellen, dass alle Felder in der richtigen Reihenfolge decodiert werden, damit sie dem Typ deiner Elm-Datenstruktur entsprechen.

Weitere Informationen und Beispiele findest du in der offiziellen Dokumentation von `elm-exploration/yaml`.

## Siehe auch

- [Offizielle Dokumentation von `elm-exploration/yaml`](https://package.elm-lang.org/packages/elm-community/yaml/latest)
- [YAML-Spezifikation](https://yaml.org/spec/)
- [YAML Syntax Cheatsheet](https://learnxinyminutes.com/docs/yaml/)