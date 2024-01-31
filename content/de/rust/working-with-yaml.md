---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML, "YAML Ain't Markup Language", ist ein benutzerfreundliches Datenformat für Datenstrukturen. Programmierer nutzen YAML wegen seiner Lesbarkeit und Einfachheit für Konfigurationsdateien, Daten-Austausch oder -Speicherung.

## How to:
Um mit YAML in Rust zu arbeiten, ist `serde_yaml` die gängige Wahl. Hier ein Beispiel:

```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    title: String,
    owner: Owner,
}

#[derive(Debug, Serialize, Deserialize)]
struct Owner {
    name: String,
    dob: String,  // im Format YYYY-MM-DD
}

fn main() -> Result<(), serde_yaml::Error> {
    // Ein YAML-String
    let data = "
title: Mein Projekt
owner:
  name: Max Mustermann
  dob: 1990-07-15
";

    // YAML-String in Rust-Struktur umwandeln
    let deserialized_data: Config = serde_yaml::from_str(data)?;

    // Ausgabe
    println!("{:?}", deserialized_data);

    Ok(())
}
```

Beispiel-Ausgabe:
```
Config { title: "Mein Projekt", owner: Owner { name: "Max Mustermann", dob: "1990-07-15" } }
```

## Deep Dive
YAML entstand Anfang der 2000er als einfachere Alternative zu XML. Für Rust gibt's neben `serde_yaml` auch `yaml-rust`, aber `serde_yaml` ist durch seine Integration mit `serde`, dem Serialization-Framework, beliebter. Die Hauptaufgabe beim Arbeiten mit YAML ist Parsing und Serialization, wobei die Typsicherheit von Rust Stärken ausspielt.

## See Also
- Offizielle Webseite von YAML: https://yaml.org 
- `serde_yaml` Crate: https://crates.io/crates/serde_yaml 
- Serde Projekt: https://serde.rs 

Für tiefergehende Konzepte und mehr Beispiele, schaut in der offiziellen `serde_yaml`-Dokumentation und dem Serde Guide nach.
