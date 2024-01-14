---
title:                "Elm: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-json.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie mit Webanwendungen arbeiten, werden Sie oft auf JSON-Daten stoßen. JSON (JavaScript Object Notation) ist ein Format zum Speichern und Übertragen von strukturierten Daten. Es ist besonders nützlich, wenn Sie Daten zwischen Client und Server austauschen müssen. In dieser Blog-Post werden wir uns damit beschäftigen, wie Sie mit JSON in Elm arbeiten können.

# Wie Geht Das

Um mit JSON in Elm zu arbeiten, müssen Sie zunächst das Elm-Paket "elm/json" installieren. Dann müssen Sie in Ihren Code das Modul "Json.Decode" importieren. In diesem Modul finden Sie verschiedene Funktionen zum Dekodieren von JSON-Daten.

```elm
import Json.Decode exposing (..)

-- Beispiel JSON-Daten
jsonData = """
    {
        "name": "Max Mustermann",
        "age": 30,
        "hobbies": ["Cooking", "Reading", "Hiking"],
        "address": {
            "street": "Hauptstraße 10",
            "city": "Berlin"
        }
    }
"""

-- Dekodieren der JSON-Daten
decodeData = decodeString (field "name" string) jsonData
-- Ausgabe: Ok "Max Mustermann"

-- Dekodieren von verschachtelten Daten
decodeAddress = decodeString (field "address" (map2 Address (field "street" string) (field "city" string))) jsonData
-- Ausgabe: Ok (Address "Hauptstraße 10" "Berlin")
```

In diesem Beispiel nutzen wir die Funktion "field", um auf die einzelnen Felder der JSON-Daten zuzugreifen. Für einfache Werte wie ein String oder eine Zahl können wir direkt die jeweilige Funktion (z.B. "string" oder "int") verwenden. Für verschachtelte Daten müssen wir die Funktion "map2" verwenden, um eine benutzerdefinierte Funktion zu erstellen, die die einzelnen Felder der JSON-Daten abruft.

# Tiefer Einblick

Neben den Grundfunktionen gibt es auch noch weitere hilfreiche Funktionen in "Json.Decode". Zum Beispiel können Sie mit der Funktion "at" auf ein bestimmtes Feld in einer verschachtelten Struktur zugreifen, anstatt "field" mehrmals zu verwenden. Außerdem gibt es Funktionen wie "oneOf", mit der Sie verschiedene mögliche Dekodierungen ausprobieren können. Stöbern Sie in der Dokumentation, um alle Möglichkeiten zu entdecken.

# Siehe Auch

- [Offizielle Elm-Dokumentation zu JSON](https://guide.elm-lang.org/effects/json.html)
- [Elm-Paket "elm/json"](https://package.elm-lang.org/packages/elm/json/latest/)
- [Interaktives Übungstool für das Arbeiten mit JSON in Elm](https://elm-shards.com/json)