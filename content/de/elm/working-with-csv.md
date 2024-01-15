---
title:                "Arbeiten mit CSV"
html_title:           "Elm: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich mit CSV beschäftigen möchte. Zum Beispiel kann man damit Daten in einem einfachen und strukturierten Format speichern und austauschen. Oder man möchte CSV-Dateien nutzen, um Daten in einer offline Umgebung zu bearbeiten und später wieder zu importieren. In jedem Fall bietet Elm eine benutzerfreundliche und effiziente Möglichkeit, mit CSV umzugehen.

## Wie

Um mit CSV in Elm zu arbeiten, können wir die Module [elm-csv](https://package.elm-lang.org/packages/elm-tools/csv/latest/) und [elm-csv-decode](https://package.elm-lang.org/packages/elm-explorations/csv/latest/) verwenden. Zuerst müssen wir diese Module in unserem Code importieren:

```Elm
import Csv
import Csv.Decode
```

Dann können wir eine CSV-Datei laden und die Daten entschlüsseln. Nehmen wir zum Beispiel an, dass wir eine Datei mit den Informationen unserer Kunden haben, in der jeder Kunde in einer Zeile gespeichert ist, mit Name, Alter und Adresse, getrennt durch ein Komma. Wir können die Daten dann wie folgt entschlüsseln:

```Elm
csvContent : Csv.FieldsDecoder Customer
csvContent =
    Decode.map4 Customer
        (Decode.field Csv.string)
        (Decode.field Csv.int)
        (Decode.field Csv.string)
        (Decode.field Csv.string)

customers : List Customer
customers =
    Csv.Decode.fromString customerscsv csvContent
```

Die Funktion `Decode.map4` erwartet eine Funktion, die mit vier Werten als Argument aufgerufen wird und einen neuen Wert zurückgibt. In unserem Fall haben wir die Funktion `Customer`, die wir selbst definieren müssen, um aus den decodierten CSV-Daten ein benutzerdefiniertes Datentyp zu erstellen.

## Deep Dive

Es gibt noch viel mehr Möglichkeiten, CSV in Elm zu nutzen. Zum Beispiel können wir die Daten aus einer CSV-Datei in ein Modell umwandeln, um sie in einer komplexeren Anwendung zu verwenden. Oder wir können die enthaltene Funktion `Decode.log` verwenden, um Fehler beim Entschlüsseln von CSV-Daten zu finden und zu beheben. Mit Elm können wir CSV-Daten sogar in einem Browser rendern, um sie anzuzeigen oder zu bearbeiten.

## Siehe auch

- Offizielle Elm-Dokumentation zu [elm-csv](https://package.elm-lang.org/packages/elm-tools/csv/latest/)
- Offizielle Elm-Dokumentation zu [elm-csv-decode](https://package.elm-lang.org/packages/elm-explorations/csv/latest/)
- [elm-live](https://github.com/wking-io/elm-live) - Eine bequeme Möglichkeit, Elm-Code in Echtzeit zu testen und anzuzeigen