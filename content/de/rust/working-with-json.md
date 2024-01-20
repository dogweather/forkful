---
title:                "Arbeiten mit JSON"
html_title:           "Rust: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?
JSON ist ein gängiges Datenformat, das häufig von Programmierern verwendet wird, um Daten zu speichern und auszutauschen. Es steht für JavaScript Object Notation und ist eine einfache und effektive Möglichkeit, strukturierte Daten zu speichern, die von verschiedenen Programmiersprachen interpretiert werden können. 
Programmierer nutzen JSON, um Daten zwischen verschiedenen Anwendungen oder Diensten auszutauschen oder um Daten in ihren Programmen zu speichern.

## Wie geht's?
Um mit JSON in Rust zu arbeiten, gibt es einige praktische Funktionen und Bibliotheken. Hier ist ein Beispiel, um eine JSON-Datei zu lesen und die enthaltenen Daten anzuzeigen:

```Rust
use serde_json::{from_str, Value};

let data = r#"{
    "name": "Max Mustermann",
    "age": 28,
    "hobbies": [
        "programming",
        "reading",
        "hiking"
    ]
}"#;

// JSON-Daten in ein Rust-Value-Objekt umwandeln
let value: Value = from_str(data).unwrap();

// Werte auslesen und ausgeben
let name = &value["name"];
let age = &value["age"];
let hobbies = &value["hobbies"];

println!("Name: {}", name); // Name: Max Mustermann
println!("Alter: {}", age); // Alter: 28
println!("Hobbys: {:?}", hobbies); // Hobbys: ["programming", "reading", "hiking"]
```

## Tiefsee-Tauchen
JSON wurde in den 1990er-Jahren entwickelt und ist ein Subset von JavaScript. Heutzutage wird es aber auch von vielen anderen Programmiersprachen unterstützt, darunter auch Rust. 
Es gibt auch andere Datenformate wie XML oder YAML, aber JSON ist aufgrund seiner Einfachheit und Lesbarkeit immer noch sehr beliebt. 
Die Verwendung von Bibliotheken wie serde_json oder json-rust erleichtert das Arbeiten mit JSON in Rust, da sie das Parsen und Erstellen von JSON-Objekten einfacher machen.

## Siehe auch
- [serde_json Dokumentation](https://docs.rs/serde_json/1.0.64/serde_json/)
- [JSON-Tutorial für Anfänger](https://www.w3schools.com/js/js_json_intro.asp)