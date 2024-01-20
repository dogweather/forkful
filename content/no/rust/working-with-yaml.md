---
title:                "Arbeid med YAML"
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language", er et menneskelesbart data-serieliseringsformat. Programmere bruker YAML for konfigurasjonsfiler og datautveksling fordi det er lett å forstå og skrive.

## How to:
For å jobbe med YAML i Rust, må du bruke `serde_yaml`-biblioteket. Installér det ved å legge til `serde_yaml = "0.8.23"` i `Cargo.toml`. Her er et eksempel på hvordan parse en YAML-streng:

```Rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    title: String,
    owner: Owner,
}

#[derive(Debug, Serialize, Deserialize)]
struct Owner {
    name: String,
    dob: String,  // Dato format: YYYY-MM-DD
}

fn main() {
    let yaml_str = r#"
        title: "Eksempel Konfig"
        owner:
          name: "Ola Nordmann"
          dob: "1990-05-30"
    "#;

    let config: Config = serde_yaml::from_str(&yaml_str).unwrap();

    println!("{:?}", config);
}
```

Kjør dette programmet og se strukturen printet ut:

```
Config {
    title: "Eksempel Konfig",
    owner: Owner {
        name: "Ola Nordmann",
        dob: "1990-05-30"
    }
}
```

## Deep Dive
YAML ble lansert i 2001 og er ofte sammenlignet med JSON og XML. Alternativer inkluderer TOML og JSON. YAML skinner hvor menneskelig redigering og lesbarhet er viktig. Implementasjonsdetaljer er viktige fordi YAML har flere feller – som tabulatormellomrom og indentering – som kan føre til feil i parsing.

## See Also
- YAML offisiell side: [yaml.org](https://yaml.org)
- Serde YAML crate-dokumentasjon: [docs.rs/serde_yaml](https://docs.rs/serde_yaml)
- Rust serialisering med Serde: [serde.rs](https://serde.rs)