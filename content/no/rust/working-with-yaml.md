---
title:                "Rust: Jobbe med yaml"
simple_title:         "Jobbe med yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en utvikler som jobber med dataintensive applikasjoner, har du mest sannsynlig støtt på YAML filer. YAML (YAML Ain't Markup Language) er et tekstformat som brukes til å representere datastrukturer på en lesbar og menneskelig måte. Det er spesielt nyttig for å beskrive konfigurasjoner og metadata for applikasjoner. Rust er et stadig mer populært programmeringsspråk som er kjent for sin ytelse og sikkerhet, og også støtter bearbeiding av YAML filer. La oss utforske hvordan du kan bruke Rust for å jobbe med YAML filer.

## Hvordan

For å kunne håndtere YAML filer i Rust, trenger vi et bibliotek som kan hjelpe oss med å tolke og manipulere dataene. Et av de beste bibliotekene for dette formålet er serde_yaml. For å bruke dette biblioteket i prosjektet ditt, må du legge til følgende linje i Rusts manifestfil (Cargo.toml):

```
serde_yaml = "0.8.16"
```

Det neste trinnet er å importere biblioteket i koden din ved å legge til følgende linje i toppen av filen din:

```
use serde_yaml;
```

Nå kan vi begynne å jobbe med YAML filen ved å åpne den med Rusts standard bibliotekfs-funksjonen. La oss se på et eksempel på hvordan vi kan lese innholdet i en YAML fil:

```
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut yaml_file = File::open("example.yaml").unwrap();
    let mut contents = String::new();
    yaml_file.read_to_string(&mut contents).unwrap();
    let data: serde_yaml::Value = serde_yaml::from_str(&contents).unwrap();
    println!("{:?}", data);
}
```

Når du kjører dette eksempelet, vil du se at den YAML filen du har åpnet, blir konvertert til en serde_yaml::Value struktur som du kan jobbe med. Fra nå av er det bare fantasien som setter grenser, og du kan begynne å manipulere dataene slik du vil.

## Dypdykk

Serde_yaml-biblioteket gir oss også muligheten til å serialisere og deserialisere Rust strukturer direkte til og fra YAML filer. Dette betyr at du kan bruke YAML som en konfigurasjonsfil for din Rust-applikasjon og enkelt endre verdier uten å måtte endre koden din. La oss se på et eksempel på hvordan dette kan gjøres:

```
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    hobbies: Vec<String>,
    contact_info: HashMap<String, String>,
}

fn main() {
    let person = Person {
        name: "Nils".to_string(),
        age: 30,
        hobbies: vec!["programming".to_string(), "hiking".to_string()],
        contact_info: [("email".to_string(), "nils@eksempel.com".to_string()),
                       ("phone".to_string(), "12345678".to_string())]
            .iter().cloned().collect(),
    };
    let yaml_string = serde_yaml::to_string(&person).unwrap();
    println!("{}", yaml_string);
}
```

Dette eksempelet viser hvordan du kan bruke serde_yaml-biblioteket til å serialisere en Rust-struktur til en YAML tekststreng som du kan bruke som en konfigurasjonsfil for din applikasjon.

## Se også

- [https://github.com/serde-rs/yaml](https://github.com/serde-rs/yaml) - Serde_yaml bibliotekets GitHub-side
- [https://docs.rs/serde_yaml/](https://docs.rs/serde_yaml/) - Serde_yaml dokumentasjon
- [https://doc.rust-lang.org/std/fs/index.html](https://doc.rust-lang.org/std/fs/index.html) - Rust standard bibliotekfs-dokumentasjon