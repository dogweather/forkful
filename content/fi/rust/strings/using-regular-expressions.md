---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:07.896724-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet eli regex mahdollistavat kehitt\xE4\
  jille merkkijonojen haun, vastaavuuden tarkistuksen ja manipuloinnin edistyneill\xE4\
  \u2026"
lastmod: '2024-03-13T22:44:56.346889-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet eli regex mahdollistavat kehitt\xE4jille\
  \ merkkijonojen haun, vastaavuuden tarkistuksen ja manipuloinnin edistyneill\xE4\
  \u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Säännölliset lausekkeet eli regex mahdollistavat kehittäjille merkkijonojen haun, vastaavuuden tarkistuksen ja manipuloinnin edistyneillä kuviohakutekniikoilla. Ruostessa (Rust), regexin hyödyntäminen auttaa tehokkaasti jäsentämään ja käsittelemään tekstidataa, mikä tekee tehtävistä, kuten datan validoinnin, haun ja tekstimuunnosten suorittamisen virtaviivaisemmin ja ylläpidettävämmin.

## Kuinka:

Rustin `regex` kirjasto on se mihin turvaudutaan työskenneltäessä säännöllisten lausekkeiden kanssa. Käyttääksesi sitä, sinun tulee ensin lisätä se `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
regex = "1"
```

Sen jälkeen voit aloittaa regex-toiminnallisuuksien toteuttamisen Ruostekoodissasi. Tässä on kuinka suorittaa joitakin yleisiä toimintoja:

### Mallin Vastaavuuden Tarkistaminen Merkkijonossa

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("Vastaako teksti päivämäärän mallia? {}", re.is_match(date));
    // Tuloste: Vastaako teksti päivämäärän mallia? true
}
```

### Osumien Etsiminen ja Saavuttaminen

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Kieli: {}, Vuosi: {}", &cap[1], &cap[2]);
    }
    // Tuloste:
    // Kieli: Rust, Vuosi: 2023
    // Kieli: C++, Vuosi: 2022
    // Kieli: Python, Vuosi: 2021
}
```

### Tekstin Korvaaminen

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 päivitettiin $2");

    println!("Päivitetty teksti: {}", replaced);
    // Tuloste: Päivitetty teksti: Rust päivitettiin 2023, C++ päivitettiin 2022, Python päivitettiin 2021
}
```

### Tekstin Jakaminen Regexin Avulla

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // jaetaan kaikilla muilla kuin sanamerkeillä
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("Kieli: {}", field);
    }
    // Tuloste:
    // Kieli: Rust
    // Kieli: C++
    // Kieli: Python
    // Kieli: Go
}
```

Nämä esimerkit tarjoavat perusoppaan aloittamiseen säännöllisten lausekkeiden kanssa Rustissa. Kun tarpeesi muuttuvat monimutkaisemmiksi, `regex` paketti tarjoaa runsaasti toiminnallisuutta monimutkaisia kuviohakutehtäviä ja tekstimanipulointia varten.
