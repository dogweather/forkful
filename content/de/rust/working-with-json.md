---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON (JavaScript Object Notation) ist ein leichtgewichtiges Datenformat zum Datenaustausch. Programmierer nutzen JSON, weil es menschenlesbar und einfach zu analysieren oder zu erzeugen ist, insbesondere für Web APIs.

## So geht's:
In Rust verwenden wir `serde_json` für das Arbeiten mit JSON. Beginnen wir mit der Einbindung in `Cargo.toml`:

```toml
[dependencies]
serde = "1.0"
serde_json = "1.0"
serde_derive = "1.0"
```

Jetzt zur Verarbeitung: ein JSON lesen, parsen und schreiben.

### JSON lesen & parsen

```rust
extern crate serde_json;
extern crate serde;
#[macro_use] extern crate serde_derive;

use serde_json::{Value, Error};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    is_programmer: bool,
}

fn read_json() -> Result<Person, Error> {
    let data = r#"
    {
        "name": "Anna",
        "age": 30,
        "is_programmer": true
    }
    "#;
    serde_json::from_str(data)
}

fn main() {
    match read_json() {
        Ok(p) => println!("Name ist: {}", p.name),
        Err(e) => println!("Fehler beim Parsen: {}", e),
    }
}
```

Ausgabe:

```
Name ist: Anna
```

### JSON schreiben

```rust
extern crate serde_json;
extern crate serde;
#[macro_use] extern crate serde_derive;

use serde_json::Error;

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    is_programmer: bool,
}

fn write_json(person: &Person) -> Result<String, Error> {
    serde_json::to_string(person)
}

fn main() {
    let person = Person {
        name: String::from("Anna"),
        age: 30,
        is_programmer: true,
    };

    match write_json(&person) {
        Ok(json) => println!("{}", json),
        Err(e) => println!("Fehler beim Schreiben: {}", e),
    }
}
```

Ausgabe:

```
{"name":"Anna","age":30,"is_programmer":true}
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre populär, da es eine Low-Overhead-Alternative zu XML ist. Alternativ könnten Programmierer auch andere Formate wie YAML oder Protobuf verwenden, wobei JSON wegen der breiten Unterstützung und der einfachen Integration in JavaScript-Ökosysteme oft bevorzugt wird. Die `serde_json`-Bibliothek in Rust ermöglicht es uns, mithilfe des Serde-Frameworks Rust-Datenstrukturen zu serialisieren und zu deserialisieren, unterstützt dabei auch komplexe und verschachtelte Datenstrukturen.

## Siehe auch
- Serde offizielle Dokumentation: https://serde.rs/
- `serde_json` Crate Dokumentation: https://docs.serde.rs/serde_json/
- Rust Programmierhandbuch: https://doc.rust-lang.org/book/
