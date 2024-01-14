---
title:                "Rust: Työskentely yamlin kanssa"
simple_title:         "Työskentely yamlin kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi aloittaa YAML-kehitys Rustilla?

YAML (yet another markup language) on tietojen esitysmuoto, joka on suunniteltu helppokäyttöiseksi ja ymmärrettäväksi ihmisille. Rust on moderneja ja tehokas ohjelmointikieli, joka on suosittu erityisesti sen turvallisuuden ja suorituskyvyn vuoksi. Yhdistäessäsi nämä kaksi, saat erittäin tehokkaan ja luotettavan tavan käsitellä YAML-tiedostoja.

## Kuinka käyttää YAML:ia Rustissa?

YAML:in käsittely Rustissa on helppoa käyttämällä cratea nimeltä "serde_yaml". Se tarjoaa työkalut tiedon lukemiseen ja kirjoittamiseen YAML-tiedostoon.

Ensiksi, lisää seuraava rivi Cargo.toml-tiedoston [dependencies] -osaan:

```Rust
serde_yaml = "0.8.11"
```

Sitten voit alkaa käyttää koodissasi serde_yaml -cratea. Alla on esimerkki koodista, jossa avataan YAML-tiedosto ja tulostetaan sen sisältö:

```Rust
use serde_yaml::Value;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("data.yaml").expect("Tiedostoa ei löytynyt!");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Tiedoston lukeminen epäonnistui!");

    let value: Value = serde_yaml::from_str(&contents)
        .expect("Tiedoston lukeminen/YAML-kääntäminen epäonnistui!");

    println!("{:#?}", value); //tulostaa YAML-tiedoston sisällön kauniimmin muotoiltuna
}
```

Kun suoritat tämän koodin, saat tulosteena YAML-tiedoston sisällön.

## Syvyyssukellus YAML:in käsittelyyn

Saatavilla on myös muita crateja, joissa voi olla lisäominaisuuksia, kuten YAML-validointi ja virheiden käsittely. Lisäksi serde_yaml -cratessa on myös mahdollisuus serialisoida Rust-struct -muotoiseksi, jolloin tiedoston sisällön muokkaaminen ja tallentaminen on helpompaa.

On myös tärkeää huomata, että YAML:in sijasta voit käyttää myös TOML- tai JSON-formaatteja serde_something-crateilla. Lisäksi saatat haluta käyttää serde -cratea yhdessä serde_yaml -craten kanssa, jotta voit käsitellä muita dataformaatteja samassa ohjelmassa.

## Katso myös

- [serde_yaml crate](https://crates.io/crates/serde_yaml)
- [serde crate](https://crates.io/crates/serde)
- [yaml-rust crate](https://crates.io/crates/yaml-rust)