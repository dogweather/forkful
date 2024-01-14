---
title:                "Rust: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi käyttää JSON:ia Rustissa?

JSON (JavaScript Object Notation) on yksinkertainen ja selkeä tapa tallentaa ja vaihtaa tietoja ohjelmien välillä. Se on myös yleisesti käytetty formaatti web-kehityksessä. Rust-ohjelmoijina meillä on mahdollisuus käyttää JSON:ia tehokkaasti ja turvallisesti, kiitos Rustin omien kaltaisten ominaisuuksien.

## Kuinka työskennellä JSON:in kanssa

Käyttäessämme JSON:ia Rustissa, meidän täytyy ensin lisätä ulkopuolinen kirjasto, kuten serde_json. Tämä mahdollistaa JSON-tiedon parsinnan ja luomisen.

```Rust
extern crate serde;
extern crate serde_json;
// tuodaan tarvittavat moduulit
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::prelude::*;

// Määritellään struct, johon tallennetaan JSON-data
#[derive(Serialize, Deserialize)]
struct Henkilo {
    nimi: String,
    ikä: u8,
    harrastukset: Vec<String>,
    kotipaikka: String,
}

fn main() {
    // Luetaan JSON-tiedosto ja tallennetaan se Henkilo-structiin
    let tiedosto = File::open("henkilot.json").expect("Tiedoston avaaminen epäonnistui.");
    let data: Henkilo = serde_json::from_reader(tiedosto).expect("JSON-data ei voitu parsia.");
    println!("Nimi: {}", data.nimi);
    println!("Ikä: {}", data.ikä);
    println!("Harrastukset: {:?}", data.harrastukset);
    println!("Kotipaikka: {}", data.kotipaikka);
    
    // Luo uusi JSON-tiedosto ja tallenna Henkilo-structin tiedot siihen
    let data = Henkilo {
        nimi: String::from("Matti Meikäläinen"),
        ikä: 35,
        harrastukset: vec![String::from("juokseminen"), String::from("kirjoittaminen")],
        kotipaikka: String::from("Tampere"),
    };
    let tiedosto = File::create("uusi_henkilo.json").expect("Tiedoston luominen epäonnistui.");
    serde_json::to_writer(tiedosto, &data).expect("JSON-dataa ei voitu kirjoittaa tiedostoon.");
}
```

Esimerkkikoodi käyttää serde_json-kirjastoa, joka tarjoaa tarvittavat työkalut JSON-tiedon käsittelyyn. Huomaa myös, että käytämme serde_derive-makroa structin annotointiin. Tämä mahdollistaa structin automaattisen muunnoksen JSON-dataksi ja päinvastoin.

## Syventävä sukellus JSON:in käsittelyyn

JSON-objekteilla on avain-arvo -parit, ja nämä parit tallennetaan HashMap-rakenteeseen. Tämä mahdollistaa nopean ja tehokkaan tiedonhaun. JSON-datasta luettaessa HashMapin avaimet ja arvot tulee määritellä oikein, jotta tiedot tallentuvat oikein structiin.

JSON:in käsittely Rustissa vaatii myös tarkkuutta tiedostojen käsittelyssä. On tärkeää huolehtia tiedoston luomisesta ja avaamisesta, sekä datan puskuroinnista. Tämä varmistaa, että ohjelma toimii odotetulla tavalla ja välttää mahdollisia virheitä.

## Katso myös

- [serde_json-kirjaston dokumentaatio](https://docs.rs/serde_json)
- [Esimerkkiprojekti JSON-datasta lukuun ja kirjoittamiseen ](https://github.com/serde-rs/json/blob/master/examples/geojson.rs)
- [vennari's blogi artikkelisarja "Creating a JSON Parser in Rust"](http://www.velocity-labs.com/blog/2017