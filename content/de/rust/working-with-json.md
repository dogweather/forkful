---
title:                "Das Arbeiten mit Json"
html_title:           "Rust: Das Arbeiten mit Json"
simple_title:         "Das Arbeiten mit Json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Warum
JSON ist eine äußerst nützliche Möglichkeit, Daten in einem menschenlesbaren Format darzustellen. Es wird häufig in der Webentwicklung, aber auch in anderen Bereichen, zur Kommunikation und Speicherung von Daten verwendet. Die Arbeit mit JSON in Rust ermöglicht es, diese Daten effizient zu verarbeiten und zu manipulieren, was in vielen Anwendungsfällen von Vorteil sein kann.

## Wie geht’s
Die Verarbeitung von JSON in Rust ist dank der Standardbibliothek serde besonders einfach. Zunächst muss jedoch das serde crate in der Cargo.toml-Datei aufgeführt werden. Dann kann ein JSON-Dokument mithilfe des von serde bereitgestellten Makros ```serde_json::from_str``` in ein entsprechendes Rust-Objekt umgewandelt werden. Ein Beispiel dafür sieht folgendermaßen aus:

```Rust
use serde_json::from_str;

fn main() {
    let json_data = r#"{"name": "Max Mustermann", "age": 30, "hobbies": ["programming", "reading"]}"#;
    
    let obj: Value = from_str(json_data).unwrap();
    println!("Name: {}", obj["name"]);
    println!("Age: {}", obj["age"]);
    
    let hobbies = obj["hobbies"].as_array().unwrap();
    println!("Hobbies: {:?}", hobbies);
}
```

Dieser Code liest ein JSON-Dokument als String ein und wandelt es in ein Value-Objekt um, das die verschiedenen Felder und Werte des Dokuments enthält. Diese können dann über den Zugriff auf das Objekt mit eckigen Klammern und dem entsprechenden Schlüssel abgerufen werden. Hier wird die Value-Methode ```as_array``` verwendet, um ein Vektor-Objekt der Hobbies zu erhalten, das wiederum mittels ```println!``` ausgegeben wird. Das Ergebnis sollte folgende Ausgabe erzeugen:

```
Name: Max Mustermann
Age: 30
Hobbies: ["programming", "reading"]
```

## Tiefere Einblicke
Obwohl die Verarbeitung von JSON in Rust dank der serde-Bibliothek sehr einfach ist, gibt es einige Dinge, die man beachten sollte. Zum einen muss das JSON-Dokument korrekt formatiert sein, da sonst ein Fehler beim Versuch der Umwandlung in ein Rust-Objekt auftritt. Zudem sollte man sich bewusst machen, dass es verschiedene Möglichkeiten gibt, die JSON-Daten in Rust-Objekte zu transformieren – je nachdem, welches Ergebnis man erzielen möchte. So können beispielsweise mithilfe von ```serde_json::to_value``` beliebige Rust-Objekte in JSON konvertiert werden.

Es ist auch wichtig zu wissen, dass bei der Verarbeitung von JSON-Daten in Rust Leistungseinbußen auftreten können. Dies liegt meist an der Notwendigkeit, die Daten zu parsen und zu validieren. Um die Leistung zu optimieren, kann es hilfreich sein, sich mit den verschiedenen Optionen und Einstellungen von serde auseinanderzusetzen.

## Siehe auch
- [JSON in Rust verarbeiten mit serde](https://serde.rs/)
- [Cargo.toml Datei](https://doc.rust-lang.org/cargo/guide/cargo-toml-vs-package-manifest.html)
- [serde_json Crate](https://crates.io/crates/serde_json)