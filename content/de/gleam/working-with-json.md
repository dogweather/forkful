---
title:                "Gleam: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Wenn du gerne mit Daten arbeitest und die Effizienz deiner Programmierung verbessern möchtest, ist das Arbeiten mit JSON in Gleam eine großartige Option. JSON ist ein leichtgewichtiges Datenformat, das in vielen Anwendungsbereichen verwendet wird, einschließlich Webentwicklung und Datenaustausch. Durch die Verwendung von JSON in Gleam kannst du deine Daten kompakt und strukturiert verarbeiten und hast so mehr Kontrolle über deine Programmlogik.

## Anleitung

Um mit JSON in Gleam zu arbeiten, musst du zuerst das Modul `gleam/json` importieren. Dann kannst du mithilfe von Funktionen wie `fromJson` und `toJson` JSON-Daten in Gleam-Records umwandeln und umgekehrt. Hier ist ein Beispiel, wie du eine Gleam-Record in JSON umwandeln kannst:

```Gleam
import gleam/json

pub const person = {
  name: "Max Mustermann",
  age: 25,
  job: "Software Entwickler"
}

pub fn main() {
  let json = json.toJson(person)
  io.println(json)
}
```

Output:
```json
{
  "name": "Max Mustermann",
  "age": 25,
  "job": "Software Entwickler"
}
```

Umgekehrt kannst du auch JSON-Daten in Gleam-Records umwandeln, indem du die Funktion `fromJson` verwendest. Hier ist ein Beispiel:

```Gleam
import gleam/json

pub const json_data = "{ \"name\": \"Maria Meier\", \"age\": 30, \"job\": \"Projektmanager\" }"

pub fn main() {
  let person = json.fromJson(json_data)
  io.println(person)
}
```

Output:
```gleam
{
  name: "Maria Meier",
  age: 30,
  job: "Projektmanager"
}
```

## Tiefergehende Einblicke

Wenn du noch tiefer in die Welt des Arbeitens mit JSON in Gleam eintauchen möchtest, gibt es noch einige weitere Funktionen und Techniken, die du erforschen kannst. Beispielsweise kannst du mit der Funktion `decode` JSON-Daten in benutzerdefinierten Gleam-Records dekodieren und mithilfe von `encode` Gleam-Records in benutzerdefinierten JSON-Formaten codieren. Außerdem gibt es noch verschiedene Funktionen, um spezifische Teile von JSON-Daten auszulesen oder zu bearbeiten.

Eine besondere Funktion, die es dir ermöglicht, Gleam-Datenstrukturen direkt in deinen JSON-Code einzubinden, ist `gleam/json`'s `json!` Makro. Hier ein Beispiel:

```Gleam
import gleam/json

pub const person = json!({
  name: "Lisa Schmidt",
  age: 28,
  job: "Designer"
})

pub fn main() {
  let json = json.toJson(person)
  io.println(json)
}
```

Output:
```json
{
  "name": "Lisa Schmidt",
  "age": 28,
  "job": "Designer"
}
```

## Siehe auch

- Weitere Informationen zu Gleam und JSON findest du in der offiziellen [Gleam Dokumentation](https://gleam.run/book/tour/json.html).
- [JSON](https://de.wikipedia.org/wiki/JavaScript_Object_Notation) Dokumentation auf Wikipedia für einen allgemeinen Überblick über das Datenformat.
- [JSON Syntax Validator](https://jsonlint.com/) um sicherzustellen, dass deine JSON-Daten syntaktisch korrekt sind.