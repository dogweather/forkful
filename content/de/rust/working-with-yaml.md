---
title:                "Rust: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum YAML in einem Rust-Projekt verwenden?

YAML ist eine einfache, menschenlesbare Datenstrukturierungssprache, die häufig für Konfigurationsdateien verwendet wird. Mit der Integration von YAML in Rust können Entwickler ihre Programmkonfigurationen in einer benutzerfreundlichen Art und Weise verwalten. Dies macht die Verwendung von YAML in Rust-Projekten besonders attraktiv.

## Anleitung: YAML in Rust verwenden

Um YAML in Ihrem Rust-Projekt zu verwenden, müssen Sie zuerst die YAML-Bibliothek in Ihrem Code hinzufügen. Fügen Sie dazu in Ihrer Cargo.toml-Datei unter [dependencies] die folgende Zeile hinzu:

```Rust
yaml = "0.4.5"
```

Sobald Sie die Bibliothek hinzugefügt haben, können Sie das `Yaml`-Objekt nutzen, um Daten in YAML-Format zu laden oder zu schreiben. Hier ist ein Beispiel, wie Sie eine YAML-Datei in Rust parsen und ihre Inhalte ausgeben können:

```Rust
use std::fs::File;
use std::io::prelude::*;
use yaml::{YamlLoader, Yaml};

fn main() {
    let mut file = File::open("example.yaml").expect("unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("unable to read file");

    let docs = YamlLoader::load_from_str(&contents).expect("unable to parse YAML");
    let doc = &docs[0];
    
    // Output the parsed YAML document
    println!("{:?}", doc);
    
    // Access specific fields in the document
    println!("Title: {}", &doc["title"]);
    println!("Author: {}", &doc["author"]);
    println!("Description: {}", &doc["description"]);

    // Output YAML document to a new file
    let mut new_file = File::create("output.yaml").expect("unable to create file");
    let yaml_string = doc.to_string();
    new_file.write_all(yaml_string.as_bytes()).expect("unable to write file");
}
```

Dieses Beispiel verwendet die externe Bibliothek `yaml-rust` und demonstriert, wie Sie eine YAML-Datei parsen, darauf zugreifen und sie schließlich in eine neue Datei schreiben können.

## Tiefer Einblick: Fortgeschrittene Verarbeitung von YAML in Rust

Die `yaml-rust`-Bibliothek bietet eine Vielzahl von Funktionen und Methoden, mit denen Sie YAML-Dateien in Ihren Rust-Code integrieren können. Einige weitere nützliche Funktionen sind:

- `write_yaml`: Schreiben Sie ein `Yaml`-Objekt in eine Datei im YAML-Format.
- `YamlValue`: Enthält alle möglichen Werte, die in YAML verwendet werden können.
- `Yaml::array`: Erstellt ein `Yaml`-Objekt vom Typ Array.
- `Yaml::mapping`: Erstellt ein `Yaml`-Objekt vom Typ Mapping.

Weitere Informationen und Beispiele finden Sie in der [offiziellen Dokumentation der `yaml-rust`-Bibliothek](https://docs.rs/yaml/0.4.5/yaml/).

## Siehe auch

- Offizielle Dokumentation: [yaml-rust](https://docs.rs/yaml/0.4.5/yaml/)
- Beispielprojekt: [rust-yaml-example](https://github.com/some-user/rust-yaml-example)
- Tutorial: [How to Use YAML in Rust](https://dev.to/someuser/how-to-use-yaml-in-rust-4h5j)