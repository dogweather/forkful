---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:38.985009-07:00
description: "Wie: Rust unterst\xFCtzt YAML nicht in seiner Standardbibliothek, daher\
  \ verwenden wir h\xE4ufig Drittanbieter-Crates wie `serde` (zum Serialisieren und\u2026"
lastmod: '2024-03-13T22:44:53.693201-06:00'
model: gpt-4-0125-preview
summary: "Rust unterst\xFCtzt YAML nicht in seiner Standardbibliothek, daher verwenden\
  \ wir h\xE4ufig Drittanbieter-Crates wie `serde` (zum Serialisieren und Deserialisieren\
  \ von Daten) in Kombination mit `serde_yaml`."
title: Arbeiten mit YAML
weight: 41
---

## Wie:
Rust unterstützt YAML nicht in seiner Standardbibliothek, daher verwenden wir häufig Drittanbieter-Crates wie `serde` (zum Serialisieren und Deserialisieren von Daten) in Kombination mit `serde_yaml`.

Füge zuerst Abhängigkeiten zu deiner `Cargo.toml` hinzu:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Jetzt sehen wir uns an, wie man einen YAML-String in eine Rust-Struktur deserialisiert und eine Rust-Struktur wieder in einen YAML-String serialisiert.

### YAML in Rust-Strukturen deserialisieren
Definiere eine Rust-Struktur, die die Daten widerspiegelt, die du in YAML erwartest. Verwende Serde-Attribute zur Anpassung, falls benötigt.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

Die Beispiel-Ausgabe beim Ausführen des oben genannten Rust-Codes wäre:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Rust-Strukturen in YAML serialisieren
Dieses Beispiel nimmt die `Config`-Struktur aus dem vorherigen Abschnitt und serialisiert sie zurück ins YAML-Format.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

Die erwartete Ausgabe ist ein im YAML-Format formatierter String:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Diese Schnipsel demonstrieren, wie du YAML-Parsing und -Generierung effizient in deinen Rust-Anwendungen integrieren kannst, unter Verwendung der beliebten Crates `serde` und `serde_yaml`. Sie bewältigen komplexe Datenstrukturen und bieten einfache, menschenlesbare Konfigurationen.
