---
aliases:
- /no/rust/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:54.461809-07:00
description: "I Rust-programmering handler det \xE5 arbeide med YAML (YAML Ain't Markup\
  \ Language) om \xE5 analysere og generere data i YAML-format, en menneskevennlig\u2026"
lastmod: 2024-02-18 23:08:53.705058
model: gpt-4-0125-preview
summary: "I Rust-programmering handler det \xE5 arbeide med YAML (YAML Ain't Markup\
  \ Language) om \xE5 analysere og generere data i YAML-format, en menneskevennlig\u2026"
title: Arbeider med YAML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I Rust-programmering handler det å arbeide med YAML (YAML Ain't Markup Language) om å analysere og generere data i YAML-format, en menneskevennlig standard for dataserielisering. Programmerere integrerer YAML-håndtering i Rust for å konfigurere applikasjoner, håndtere innstillinger, eller behandle komplekse datastrukturer i et klart og lesbart format, og utnytter dets enkelhet over JSON eller XML for konfigurasjonsfiler og datautveksling.

## Hvordan:

Rust støtter ikke YAML i sitt standardbibliotek, så vi bruker vanligvis tredjepartsbiblioteker som `serde` (for serialisering og deserialisering av data) i kombinasjon med `serde_yaml`.

Først, legg til avhengigheter i din `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Nå, la oss se på hvordan man deserialiserer en YAML-streng til en Rust-struktur og serialiserer en Rust-struktur tilbake til en YAML-streng.

### Deserialisere YAML til Rust-strukturer

Definer en Rust-struktur som speiler dataene du forventer i YAML. Bruk Serde-attributter for tilpasning om nødvendig.

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

Eksempelutdata ved kjøring av ovennevnte Rust-kode vil være:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Serialisere Rust-strukturer til YAML

Dette eksemplet tar `Config`-strukturen fra forrige seksjon og serialiserer den tilbake til YAML-format.

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

Forventet utdata vil være en YAML-formatert streng:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Disse kodestykkene demonstrerer hvordan du effektivt kan integrere YAML-analyse og -generering i dine Rust-applikasjoner, ved bruk av de populære `serde` og `serde_yaml`-bibliotekene, imøtekomme komplekse datastrukturer og tilby enkle, menneskelesbare konfigurasjoner.
