---
title:                "Rust: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es viele verschiedene Datenformate, die verwendet werden können. Eines der am häufigsten verwendeten Formate ist JSON. In dieser Blog-Post geht es darum, wie man mit JSON in der Programmiersprache Rust arbeitet und welche Vorteile dies hat.

## Wie man mit JSON in Rust arbeitet

Rust bietet zahlreiche Bibliotheken und Funktionen zur Verarbeitung von JSON. Zunächst müssen Sie jedoch die `serde`- und `serde_json`-Bibliotheken in Ihrem Projekt importieren. Danach können Sie die `json::parse`-Funktion verwenden, um eine JSON-Zeichenfolge in ein `serde_json::Value`-Objekt umzuwandeln.

```Rust
use serde_json::Value;

let json_string = "{\"name\": \"Max\", \"age\": 25}";
let json_value: Value = json::parse(json_string).expect("Failed to parse JSON.");

// Zugriff auf Werte
let name = json_value["name"].as_str().expect("Name not found.")
let age = json_value["age"].as_u64().expect("Age not found.")

println!("Name: {}", name);
println!("Age: {}", age);

// Erzeugen von JSON aus Rust-Daten
let mut json_object = serde_json::Map::new();
json_object.insert("city".to_string(), serde_json::Value::String("Berlin".to_string()));
json_object.insert("country".to_string(), serde_json::Value::String("Germany".to_string()));

let json_value = serde_json::Value::Object(json_object);
println!("JSON Output: {}", json_value);
```

Dieses Beispiel zeigt, wie man eine JSON-Zeichenfolge in ein `Value`-Objekt umwandelt und auf die enthaltenen Werte zugreift. Ebenso wird gezeigt, wie man aus Rust-Daten ein `Value`-Objekt erzeugt und als JSON ausgeben kann.

## Tiefergehende Informationen zur Arbeit mit JSON

In Rust gibt es verschiedene Möglichkeiten, mit JSON zu arbeiten. Neben der `serde`- und `serde_json`-Bibliothek gibt es auch die `json`-Bibliothek, die eine `Json`-Struktur bereitstellt, die direkt mit JSON-Werten ausgetauscht werden kann. Diese Bibliothek ist besonders nützlich, wenn es um das Parsen von JSON geht, während `serde` und `serde_json` eher für die Verarbeitung von komplexeren JSON-Strukturen geeignet sind.

Eine weitere nützliche Funktion von Rust im Umgang mit JSON ist die `from_str`-Methode der `serde_json::Value`-Struktur. Diese ermöglicht es, eine JSON-Zeichenfolge direkt in eine rusteigene Datenstruktur umzuwandeln, sodass der Zugriff auf die Werte noch einfacher wird.

## Siehe auch

- [Die offizielle Rust-Dokumentation zu JSON](https://doc.rust-lang.org/std/convert/trait.From.html)
- [Ein Tutorial auf Deutsch zu Rust und JSON](https://www.bleeptrack.de/tutorials/rust-json/)
- [Ein Tutorial auf Englisch zu Rust und JSON](https://erwabook.com/intro.html)

Vielen Dank, dass Sie diesen Blog-Beitrag gelesen haben. Wir hoffen, dass er Ihnen bei der Arbeit mit JSON in Rust geholfen hat und Sie nun die Vorteile dieser Kombination nutzen können. Happy coding!