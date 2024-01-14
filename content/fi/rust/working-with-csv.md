---
title:                "Rust: Työskentelyä csv:n kanssa"
simple_title:         "Työskentelyä csv:n kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää Rustia CSV:n kanssa

Rust on yleiskäyttöinen ohjelmointikieli, joka tarjoaa suorituskykyä ja turvallisuutta. Se on myös erittäin hyödyllinen työskennellessä CSV-tiedostojen kanssa.

## Kuinka tehdä se

Tämä on esimerkki yksinkertaisesta skriptistä, joka lukee CSV-tiedoston ja tulostaa sen sisällön konsoliin:

```Rust
use std::fs::File;
use std::io::prelude::*;
use csv::Reader;

let mut file = File::open("tiedosto.csv").expect("Tiedoston avaaminen epäonnistui.");

let mut csv = Reader::from_reader(file);
for result in csv.records() { 
    let record = result.unwrap(); 
    println!("{:?}", record);
}
```

Tämä koodi käyttää `csv` kirjastoa, joka on yhteisöllisesti ylläpidetty Rust-kirjasto, joka auttaa lukemaan ja kirjoittamaan CSV-tiedostoja.

Esimerkkitulostus:

```
["Nimi", "Ikä"]
["Matti", "25"]
["Maija", "30"]
```

Voit myös kerätä CSV-tiedoston sisällön vektoriksi:

```Rust
let data: Vec<Vec<String>> = csv.records()
    .map(|r| r.unwrap().iter().map(|f| f.to_string()).collect())
    .collect();
```

Tässä esimerkissä tiedosto sisältäisi saman sisällön kuin edellinen esimerkki. Saatulistan tulisi näyttää tältä:

```
[["Nimi", "Ikä"], ["Matti", "25"], ["Maija", "30"]]
```

Voit myös käyttää `serde` kirjastoa JSON-tiedoston generoimiseen CSV-tiedoston perusteella:

```Rust
use serde::Serialize;

#[derive(Serialize)]
struct Person {
    name: String,
    age: i32,
}

let mut wtr = csv::Writer::from_path("tiedosto.csv").unwrap();
wtr.serialize(Person{name: "Matti".to_owned(), age: 25}).unwrap();
wtr.serialize(Person{name: "Maija".to_owned(), age: 30}).unwrap();
wtr.flush().unwrap();
```

Yllä oleva koodi tuottaisi seuraavan CSV-tiedoston:

```
name,age
Matti,25
Maija,30
```

## Syventävä tietoa CSV:n kanssa työskentelystä

CSV-tiedostot ovat yleisiä tietojen tallennusmuotoja, ja niitä käytetään usein tietojen siirtämiseen eri ohjelmistojen välillä. Rustin avulla CSV-tiedostojen lukeminen ja kirjoittaminen on helppoa ja turvallista, sillä se tarjoaa tarkat tietotyypit ja estää tyypittämättömien arvojen huomaamattoman käytön.

CSV-tiedostojen käsittelyyn liittyy kuitenkin myös haasteita, kuten tiedostojen erilaiset muodot ja erikoismerkit. Suosittelemme tutustumaan tarkemmin `csv` ja `serde` kirjastojen dokumentaatioon, jotta voit hyödyntää niitä tehokkaasti CSV-tiedostojen työstämisessä.

## Katso myös

- [csv-kirjaston dokumentaatio](https://docs.rs/csv)
- [serde-kirjaston dokumentaatio](https://docs.rs/serde)
- [Rustin virallinen verkkosivusto](https://www.rust-lang.org/fi/)