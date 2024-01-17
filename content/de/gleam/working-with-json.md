---
title:                "Arbeiten mit JSON"
html_title:           "Gleam: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-json.md"
---

{{< edit_this_page >}}

Gleam: JSON woordenboek

## Was & Warum?

JSON, auch bekannt als "JavaScript Object Notation", ist ein Datenformat, das häufig von Programmierern verwendet wird, um Daten zu speichern und zu übertragen. Es basiert auf der Syntax von JavaScript, ist aber unabhängig von der Programmiersprache und leicht zu lesen und zu schreiben. Programmierer verwenden JSON, um Daten zwischen verschiedenen Anwendungen und Systemen auszutauschen und zu verarbeiten, da es ein universelles Format ist, das von vielen gängigen Programmiersprachen unterstützt wird.

## Wie:

ẞeitdem Gleam ein Typsystem hat, erleichtert es die Arbeit mit JSON. Der Typ ```Json.Value``` ermöglicht es, jedes JSON-Datenobjekt zu repräsentieren. Um auf ein bestimmtes Objekt oder Attribut zuzugreifen, können wir die Gleam-Weile-Schreibweise verwenden.

Beispiel:

```Gleam
let json = Json.from_string("{ "name": "Max", "age": 25 }")
let name = json.name
let age = json.age
```

Ausgabe:

```
"Hallo Max, du bist 25 Jahre alt!"
```

Um Daten in JSON umzuwandeln, können wir die Funktion ```Json.to_string``` verwenden, die ein Gleam-JSON-Wert als Parameter akzeptiert und eine lesbare JSON-Zeichenfolge zurückgibt.

Beispiel:

```Gleam
let person = Json.from_string("{ "name": "Lisa", "age": 30 }")
let person_as_json = Json.to_string(person)
```

Ausgabe:

```
"{ "name": "Lisa", "age": 30 }"
```

## Tiefer Eintauchen:

JSON wurde ursprünglich 2001 von Douglas Crockford entwickelt und ist seitdem zu einem beliebten Format für die Übertragung von Daten zwischen Anwendungen geworden. Es wird oft als Alternative zu XML verwendet, da es kompakter und leichter zu lesen und zu schreiben ist. Auch in der heutigen Zeit wird es immer noch häufig in Webentwicklung und APIs verwendet.

Es gibt einige Alternativen zu JSON, wie z.B. YAML oder MessagePack, aber aufgrund seiner Einfachheit und Universalität bleibt JSON die bevorzugte Wahl für viele Programmierer. In Gleam ermöglicht das Typsystem eine robuste und sichere Verarbeitung von JSON-Daten.

## Siehe auch:

- [JSON Tutorial auf w3schools.com](https://www.w3schools.com/js/js_json_intro.asp)
- [Gleam-Dokumentation für JSON](https://gleam.run/articles/json/overview)