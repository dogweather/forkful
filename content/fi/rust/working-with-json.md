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

## Miksi

Miksi Rustin kanssa työskenneltäisiin JSONin kanssa? Koska JSON on yleisesti käytetty tiedonvaihtomuoto ja Rust tarjoaa tehokkaan ja turvallisen tavan käsitellä sitä.

## Miten

JSON-tiedoston lukeminen ja kirjoittaminen on helppoa Rustilla. Käytä vain "serde_json" -kirjastoa ja "serde" -makroja.

```Rust
use serde_json::{Result, Value};

fn main() -> Result<()> {
    // Luetaan JSON-tiedosto
    let data = r#"
        {
            "eläin": "koira",
            "ikä": 7,
            "omistaja": "Matti"
        }
    "#;

    // Muunna JSON-string Value-tyypiksi
    let v: Value = serde_json::from_str(data)?;

    // Hae arvot avaimilla
    let eläin = v["eläin"].as_str().unwrap();
    let ikä = v["ikä"].as_i64().unwrap();
    let omistaja = v["omistaja"].as_str().unwrap();

    // Tulosta tiedot
    println!("{} on {}-vuotias ja sen omistaa {}", eläin, ikä, omistaja);

    // Muodosta uusi JSON-tiedosto
    let uusi_tiedosto = serde_json::json!({
        "eläin": eläin,
        "ikä": ikä,
        "omistaja": omistaja
    });

    // Kirjoita tiedosto levylle
    serde_json::to_writer_pretty(&File::create("uusi_tiedosto.json")?, &uusi_tiedosto)?;

    Ok(())
}
```

Esimerkkituloste:

```
koira on 7-vuotias ja sen omistaa Matti
```

## Syventävä sukellus

Rust tarjoaa monia erilaisia tapoja käsitellä JSON-tietoja "serde_json" kirjaston avulla. Tarkkaan ottaen, "serde" -makrot tekevät koodista helpommin luettavan ja vähentävät virheitä.

## Katso myös

- [serde-json - GitHub](https://github.com/serde-rs/json)
- [Rust, JSON ja serde - Medium](https://medium.com/swlh/working-with-json-in-rust-using-serde-3bdde100d513)
- [Rust-json - Crates.io](https://crates.io/crates/rust-json)