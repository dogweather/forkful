---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"

category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Praca z YAML to zarządzanie danymi w formacie, który jest łatwiejszy dla ludzi do odczytania i pisania niż JSON lub XML. Programiści wykorzystują YAML głównie do konfiguracji projektów oraz przechowywania prostych danych.

## Jak to zrobić:
Do obsługi YAML w Rust użyjemy biblioteki `serde_yaml`. Trzeba ją dodać do `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Teraz zobaczmy przykład:

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    title: String,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    dob: String,
}

fn main() {
    let config_str = "
title: Example YAML
owner:
  name: Jan Kowalski
  dob: 1979-05-27
    ";

    let parsed_config: Config = serde_yaml::from_str(config_str).unwrap();
    println!("Tytuł: {}, Właściciel: {}", parsed_config.title, parsed_config.owner.name);
}
```

Wyjście:
```
Tytuł: Example YAML, Właściciel: Jan Kowalski
```

## Pogłębienie:

YAML (YAML Ain't Markup Language) powstał w 2001 roku jako udogodnienie dla ludzi w porównaniu do XML. Alternatywy dla YAMLa to JSON oraz TOML, z różnicami w czytelności i możliwościach. Ważne jest, że `serde_yaml` wykorzystuje Serde – potężną bibliotekę ser/deser Rusta, co pozwala na łatwą pracę z wieloma formatami danych.

## Zobacz również:

- Serde: https://serde.rs/
- Oficjalna strona YAML: https://yaml.org/
- Dokumentacja `serde_yaml`: https://docs.rs/serde_yaml/
