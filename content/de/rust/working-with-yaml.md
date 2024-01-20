---
title:                "Arbeiten mit YAML"
html_title:           "Rust: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Programmieren mit Rust begegnet man häufig dem Format YAML. YAML (Yet Another Markup Language) ist ein standardisiertes Dateiformat mit leserlicher Syntax, das insbesondere für die Speicherung von Konfigurationsdaten verwendet wird. Viele Programmierer nutzen YAML, da es einfach zu lesen, zu schreiben und zu parsen ist.

## Wie geht's?
In Rust, kann man das YAML-Format ganz einfach in ein Struct verwandeln. Hier ist ein Beispiel, wie man ein YAML-Dokument mit Hilfe von Serde in ein Struct umwandelt:

```Rust
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::prelude::*;

#[derive(Deserialize, Serialize)]
struct Config {
    username: String, 
    password: String,
    database: String,
}

fn main() {
    let mut file = File::open("config.yml").expect("Unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Unable to read file");

    let config: Config = serde_yaml::from_str(&contents).expect("Unable to parse YAML");

    println!("Username: {}", config.username);
    println!("Password: {}", config.password);
    println!("Database: {}", config.database);
}
```

Wir importieren zwei hilfreiche Bibliotheken: Serde, die es uns ermöglicht, YAML-Dateien zu parsen, und std::fs, um eine Datei zu öffnen und zu lesen. Anschließend erstellen wir ein Struct mit den entsprechenden Feldern, die in unserem YAML-Dokument vorhanden sind. Schließlich lesen wir die Datei und wandeln sie in ein Struct um, das wir dann verwenden können.

## Tiefes Eintauchen
YAML wurde im Jahr 2001 von Clark Evans entwickelt und wurde häufig für Konfigurationsdateien verwendet. Es ist eine Textdatei, die für Menschen leicht lesbar und schreibbar ist, aber auch von Maschinen leicht verarbeitet werden kann.

Es gibt verschiedene Alternativen zu YAML, wie zum Beispiel JSON oder XML. Diese sind jedoch nicht so menschenleserlich wie YAML. Wenn Sie mit Rust arbeiten, kann YAML eine bequeme Wahl sein, da es gut mit Rusts Typsystem harmoniert.

Die Implementierung von YAML in Rust erfolgt durch die library serde_yaml, die von der allgemein bekannten Serde library abhängt. Diese library ist sehr stabil und wird ständig aktualisiert, um die beste Leistung zu bieten.

## Siehe auch
- [YAML dokumentation](https://yaml.org/)
- [Serde library](https://github.com/serde-rs/serde)