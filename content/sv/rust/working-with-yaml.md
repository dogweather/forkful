---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat för konfiguration och serialization, lättläst för människor. Programmerare använder YAML för att hantera konfigfiler, kommunikation mellan tjänster och datastrukturering enkelt och tydligt.

## Hur gör man:
För att hantera YAML i Rust, använd biblioteket `serde_yaml`. Först, installera det:

```toml
serde_yaml = "0.8.23"
```

Läs in och serialisera en YAML-fil:

```rust
use serde::{Serialize, Deserialize};
use serde_yaml;
use std::error::Error;

#[derive(Debug, Serialize, Deserialize)]
struct Konfig {
    titel: String,
    storlek: i32,
    flaggor: Vec<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let data = "
titel: Exempelkonfig
storlek: 10
flaggor:
  - snabb
  - säker
";
    let konfig: Konfig = serde_yaml::from_str(data)?;
    println!("{:?}", konfig);
    Ok(())
}
```

Exempel på output:

```
Konfig { titel: "Exempelkonfig", storlek: 10, flaggor: ["snabb", "säker"] }
```

## Djupdykning
YAML, "YAML Ain't Markup Language", föddes 2001 för att vara mer läslig än XML. Alternativ inkluderar JSON och TOML. `serde_yaml` använder `serde`-biblioteket för Rusts serialisering/deserialisering, som är känt för sin effektivitet och säkerhet.

## Se också
- Serde officiell webbplats: [https://serde.rs](https://serde.rs)
- YAML-specifikationen: [https://yaml.org](https://yaml.org)
- Serde_yaml dokumentation: [https://docs.rs/serde_yaml](https://docs.rs/serde_yaml)
