---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? 
"Mitä & Miksi?"

YAML on ihmislukuisen datan serialisointiformaatti konfiguraatioille, hallintaan ja sovellusten asetuksiin. Ohjelmoijat käyttävät YAMLia, koska se on helppolukuinen, muokattava ja yleisesti tuettu eri teknologioissa.

## How to:
"Kuinka tehdä:"

Rust-ohjelmissa YAML käsitellään 'serde_yaml' kirjaston avulla:

```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    username: String,
    language: String,
}

fn main() {
    let yaml_str = r#"
username: "kayttajanimi"
language: "Finnish"
"#;
    let config: Config = serde_yaml::from_str(yaml_str).expect("Invalid YAML format");
    
    println!("Username: {}", config.username);
    println!("Language: {}", config.language);
}
```

Tuloste:
```
Username: kayttajanimi
Language: Finnish
```

## Deep Dive:
"Syvä sukellus:"

YAML (YAML Ain't Markup Language) on kehitetty vuonna 2001 XML:n yksinkertaisemmaksi vaihtoehdoksi. Rustissa, 'serde_yaml' käyttää 'serde' (serialization/deserialization) kirjastoa datan käsittelyyn, ja se on yksi suosituimmista lähestymistavoista ruostepuolella. JSON ja TOML ovat suosittuja vaihtoehtoja YAMLille, mutta YAML on erottuva sen ihmislukuisuuden ja monimutkaisempien rakenteiden esittämiskyvyn ansiosta.

## See Also:
"Katso myös:"

- Serde YAML dokumentaatio: [Serde YAML](https://docs.rs/serde_yaml/)
- YAML virallinen sivusto: [YAML](https://yaml.org)
- Rust serialization/deserialization 'serde' kirjasto: [Serde](https://serde.rs)
