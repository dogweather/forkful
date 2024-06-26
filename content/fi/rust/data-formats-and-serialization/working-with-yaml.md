---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:47.895478-07:00
description: "Kuinka: Rust ei tue YAML:\xE4\xE4 sen vakio kirjastossa, joten yleens\xE4\
  \ k\xE4yt\xE4mme kolmannen osapuolen crateja kuten `serde` (datan serialisointiin\
  \ ja\u2026"
lastmod: '2024-03-13T22:44:56.377575-06:00'
model: gpt-4-0125-preview
summary: "Rust ei tue YAML:\xE4\xE4 sen vakio kirjastossa, joten yleens\xE4 k\xE4\
  yt\xE4mme kolmannen osapuolen crateja kuten `serde` (datan serialisointiin ja deserialisointiin)\
  \ yhdess\xE4 `serde_yaml`:n kanssa."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Rust ei tue YAML:ää sen vakio kirjastossa, joten yleensä käytämme kolmannen osapuolen crateja kuten `serde` (datan serialisointiin ja deserialisointiin) yhdessä `serde_yaml`:n kanssa.

Lisää ensin riippuvuudet `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Katsotaan nyt, miten YAML-merkkijono deserialisoidaan Rust-rakenteeseen ja miten Rust-rakenne serialisoidaan takaisin YAML-merkkijonoksi.

### YAML:n Deserialisointi Rust-Rakenteisiin
Määritä Rust-rakenne, joka peilaa odottamaasi dataa YAML:ssa. Käytä Serde-attribuutteja mukauttamisen tarvittaessa.

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
name: Kilpi
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

Esimerkkituloste, kun yllä oleva Rust-koodi suoritetaan, olisi:

```plaintext
Config { name: "Kilpi", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Rust-Rakenteiden Serialisointi YAML:ksi
Tämä esimerkki ottaa edellisessä osassa `Config`-rakenteen ja serialisoi sen takaisin YAML-muotoon.

```rust
fn main() {
    let config = Config {
        name: String::from("Kirves"),
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

Odotettu tulos on YAML-muodossa oleva merkkijono:

```yaml
---
name: Kirves
durability: 120
owner:
  name: Alex
  age: 30
```

Nämä katkelmat osoittavat, miten voit tehokkaasti integroida YAML:n jäsentämisen ja tuottamisen Rust-sovelluksiisi, käyttäen suosittuja `serde`- ja `serde_yaml`-crates-paketteja, käsittellen monimutkaisia tietorakenteita ja tarjoten yksinkertaiset, ihmisen luettavat konfiguraatiot.
