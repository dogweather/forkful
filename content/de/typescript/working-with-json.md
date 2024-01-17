---
title:                "Arbeiten mit Json"
html_title:           "TypeScript: Arbeiten mit Json"
simple_title:         "Arbeiten mit Json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Arbeiten mit JSON ist ein wesentlicher Bestandteil der modernen Programmierung. JSON steht für JavaScript Object Notation, und es ist ein Format zum Speichern und Übertragen von Daten. Programmierer nutzen JSON, um Daten in einer einfachen und lesbareren Weise zu organisieren und zu transportieren.

## Wie geht's?

Das Arbeiten mit JSON ist in TypeScript einfach und unkompliziert. Um ein JSON-Objekt zu erstellen, verwenden Sie den Befehl ```JSON.parse()``` mit einem gültigen JSON-String als Eingabe. Um ein JSON-Objekt in einen String umzuwandeln, können Sie den Befehl ```JSON.stringify()``` verwenden.

```TypeScript
// Ein JSON-String erstellen
let jsonString = '{"name": "Max", "age": 25, "hobby": "programming"}';

// JSON-String in ein Objekt umwandeln
let json = JSON.parse(jsonString);

// Ein Objekt in einen JSON-String umwandeln
let newJsonString = JSON.stringify(json);
```

Die Ausgabe sieht wie folgt aus:

```TypeScript
jsonString = {"name": "Max", "age": 25, "hobby": "programming"}
json = {name: "Max", age: 25, hobby: "programming"}
newJsonString = '{"name": "Max", "age": 25, "hobby": "programming"}'
```

## Tief eintauchen

JSON wurde erstmals im Jahr 2001 von Douglas Crockford eingeführt und hat sich seitdem zu einem der am häufigsten genutzten Datenformate entwickelt. Alternativen zu JSON sind beispielsweise XML und CSV, aber JSON bleibt aufgrund seiner einfachen Syntax und seiner Kompatibilität mit JavaScript eine beliebte Wahl.

In TypeScript gibt es verschiedene Methoden zum Arbeiten mit JSON, wie zum Beispiel das Validieren von JSON-Daten durch Verwendung von Typen oder das Verwenden von JSON APIs zum Abrufen und Senden von Daten.

## Siehe auch

- [JSON vs XML: Which One is Better?](https://www.programiz.com/json-vs-xml)
- [Working with JSON in TypeScript](https://blog.logrocket.com/working-with-json-data-in-typescript/)
- [JSON in TypeScript](https://blog.logrocket.com/json-typescript/)