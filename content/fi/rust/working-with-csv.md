---
title:                "Työskentely csv:n kanssa"
html_title:           "Rust: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi aloittaa työskentelyn CSV-tiedostojen kanssa? No, ensinnäkin CSV-tiedostot ovat erittäin yleisiä tapoja tallentaa ja jakaa taulukkomuotoista dataa, joten niiden käsittelyyn liittyvät taidot ovat hyödyllisiä monilla eri aloilla, kuten ohjelmistokehityksessä, data-analytiikassa ja taloustieteessä.

## Miten

Rustin CSV-kirjasto, nimeltään "csv", tarjoaa helpon ja tehokkaan tavan käsitellä ja lukea CSV-tiedostoja. Tässä esimerkissä näytämme, kuinka lukea CSV-tiedosto ja tulostaa sen sisältö konsoliin:

```Rust
use csv::Reader;

fn main() {
    let mut csv_reader = Reader::from_path("data.csv").unwrap();
    for result in csv_reader.records() {
        let record = result.unwrap();
        println!("{:?}", record);
    }
}
```

Tämä koodi luo Reader-tyypin instanssin, joka lukee tiedoston "data.csv" ja käy läpi jokaisen rivin sisällön käyttäen `.records()` -metodia. Koodin suorittamisen jälkeen näet kaikki tiedoston rivit tulostettuna konsoliin.

## Syvempää

Kun olet perehtynyt perusteisiin, voit tutkia CSV-kirjaston muita ominaisuuksia, kuten datan kirjoittamista CSV-tiedostoon tai räätälöityjä tapoja käsitellä tietueita. Voit myös harkita muiden Rust-kirjastojen, kuten "serde", käyttöä datan lukemiseen ja muokkaamiseen erilaisissa muodoissa.

## Katso myös

- [csv Rust-kirjaston dokumentaatio](https://docs.rs/csv/)
- [Serde Rust-kirjasto](https://serde.rs/)
- [Rustin virallinen verkkosivusto](https://www.rust-lang.org/)