---
title:                "Työskentely jsonin kanssa"
html_title:           "Rust: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Työskentely JSON:n kanssa tarkoittaa tietojen muuttamista binäärimuodosta tekstipohjaiselle formaatille, jota kutsutaan JSON-muodoksi. JSON on erittäin suosittu monissa ohjelmoinnin sovelluksissa, koska siinä on yksinkertainen syntaksi ja se mahdollistaa tiedon siirtämisen eri alustojen välillä.

## Miten:
Koodin kirjoittaminen Rustissa JSON-muotoon tapahtuu käyttäen serde-kirjastoa. Esimerkiksi, kun haluat muuntaa JSON-muotoisen tekstin rakenteelliseksi datatyypiksi, voit käyttää serde-json kirjastoa.

```Rust
use serde_json::{Result,Value};

fn main() -> Result<()> {
    // JSON-muotoinen teksti
    let data = r#"
        {
            "name": "John Snow",
            "age": 28,
            "hobbies": ["fighting", "exploring"]
        }
    "#;

    // Muunnetaan teksti natiiviksi JSON-rakenteeksi
    let v: Value = serde_json::from_str(data)?;

    // Printataan JSON-rakenne konsoliin
    println!("{} is {} years old and likes {}.",
        v["name"], 
        v["age"], 
        v["hobbies"].as_array().unwrap().join(", ")
    );
    Ok(())
}
```

Tulostus:

```bash
John Snow is 28 years old and likes fighting, exploring.
```

## Syväsukellus:
JSON-muoto otettiin käyttöön jo vuonna 2001 ja siitä tuli nopeasti suosittu vaihtoehto XML:lle tiedon muodostamiseen ja siirtämiseen verkon yli. Sen yksinkertaisempi syntaksi ja pienempi tiedostokoko tekivät siitä houkuttelevan monissa sovelluksissa, erityisesti web-sovelluksissa. Vaikka JSON on yleisesti käytetty, on olemassa myös muita vaihtoehtoja, kuten YAML ja CSV.

Rustin serde-kirjasto tarjoaa monipuolisen ja tehokkaan tavan koodata ja dekoodata JSON-muotoa. Kirjasto pystyy käsittelemään monenlaisia JSON-tietorakenteita ja tukee myös tiettyjä lisäominaisuuksia, kuten järjestettyä JSON:ia ja dataa merkkijonoina.

## Katso myös:
- [serde-kirjasto](https://crates.io/crates/serde)
- [JSON-esimerkki: API-pyyntö Rustilla](https://github.com/serde-rs/json#example-making-an-api-request-from-rust)