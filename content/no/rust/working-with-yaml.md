---
title:                "Å jobbe med yaml"
html_title:           "Rust: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens verden av programvareutvikling, hvor kompleksiteten og datamengden stadig øker, har behovet for fleksible og leservennlige formater for datautveksling blitt viktigere enn noensinne. YAML er et av disse formatene, og ved å bruke Rust, et moderne og effektivt programmeringsspråk, kan du enkelt håndtere og manipulere YAML-data på en trygg og effektiv måte.

## Hvordan gjøre det

For å komme i gang med å jobbe med YAML i Rust, følg disse trinnene:

1. Installer Rust ved å følge instruksjonene på [Rust sin offisielle nettside](https://www.rust-lang.org/). Dette vil også installere Rust sin pakkebehandler, Cargo.
2. Opprett et nytt Rust-prosjekt ved å kjøre kommandoen `cargo new <prosjektnavn>`.
3. Legg til følgende avhengighet i `Cargo.toml`-filen i ditt nyopprettede prosjekt: `yaml_rs = "0.4.0"`.
4. Utforsk dokumentasjonen til [YAML-rs biblioteket](https://docs.rs/yaml-rs/0.4.0/yaml_rs/) for å lære mer om dets funksjoner og hvordan de kan brukes.
5. Bruk kommandoene `cargo build` og `cargo run` for å bygge og kjøre ditt nye prosjekt.

La oss nå se på et enkelt eksempel på hvordan du kan bruke YAML-rs biblioteket i et Rust-program:

```Rust
extern crate yaml_rs;
use std::fs::File;

fn main() {
    // Opprett en ny fil som skal inneholde YAML-data
    let mut file = File::create("data.yaml").expect("Kunne ikke opprette fil");

    // Opprett en ny YAML Data struktur
    let data = yaml_rs::Data::new()
        .add("navn", "Ola Nordmann")
        .add("alder", 35)
        .add("jobb", "Utvikler");

    // Skriv YAML-dataen til filen
    data.write_to(&mut file).expect("Kunne ikke skrive til fil");

    // Les YAML-dataen fra filen og konverter den til et hashmap
    let data_leser = yaml_rs::Reader::from_file("data.yaml").expect("Kunne ikke åpne fil");
    let leste_data = data_leser.deserialize().expect("Kunne ikke konvertere til hashmap");

    // Skriv ut dataen
    println!("{:?}", leste_data);
}
```

Dette programmet vil skrive ut følgende:

```
{"alder": 35, "navn": "Ola Nordmann", "jobb": "Utvikler"}
```

## Dykk dypere

La oss nå ta en nærmere titt på noen av de viktigste funksjonene i YAML-rs biblioteket:

### Data struktur

Som vist i eksempelet ovenfor, er `Data` strukturen hovedkomponenten i YAML-rs biblioteket. Denne strukturen kan holde alle typer data, inkludert strenger, tall og komplekse datastrukturer som hashmap.

### Reader og Writer

YAML-rs biblioteket tilbyr også en `Reader` og `Writer` klasse, som gjør det enkelt å lese og skrive YAML-data mellom filer og datastrukturer.

### Feilhåndtering

For å sikre at applikasjonen din håndterer feil på en effektiv måte, tilbyr YAML-rs biblioteket også en `YamlError` enum som kan brukes til å fange og håndtere eventuelle feil som oppstår under kjøring.

## Se også

- [Rust sin offisielle nettside](https://www.rust-lang.org/)
- [YAML-rs dokumentasjon](https://docs.rs/yaml-rs/0.4.0/yaml_rs/)