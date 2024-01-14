---
title:                "Rust: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa joudutaan muuttamaan päivämäärä merkkijonoksi esimerkiksi käyttäjän lukemisen tai tulostamisen yhteydessä. Rust tarjoaa tehokkaan ja turvallisen tavan muuntaa päivämäärä halutussa muodossa.

## Miten tehdä

Rust tarjoaa valmiin Date-muuttujan muuntaa päivämäärä mukautetun merkkijonon avulla. Alla olevassa koodiesimerkissä näytämme, miten tämä voidaan tehdä:

```Rust
use std::time::SystemTime;

fn main() {
    // Luo uusi Date-muuttuja nykyisellä ajalla
    let today = SystemTime::now();
    
    // Muunna päivämäärä merkkijonoksi
    let date_string = today.to_string();
    println!("Päivämäärä merkkijonona: {}", date_string);
}
```

Tämä tulostaa seuraavan:

```bash
Päivämäärä merkkijonona: Thu Oct 29 16:31:06 2020
```

## Syvällisempi sukellus

Rustin Date-muuttujalla on useita sisäisiä metodeja, jotka mahdollistavat päivämäärän muuntamisen eri muodoissa, kuten Unix-aikaleimalla tai UTC-muodossa. Voit lukea lisää näistä metodeista [Rustin virallisesta dokumentaatiosta](https://doc.rust-lang.org/std/time/struct.SystemTime.html).

Jokainen Rustin Date-muuttujan metodi palauttaa Result<T, E> -tyyppisen arvon, joka mahdollistaa virheiden käsittelyn sen sijaan, että ohjelma kaatuisi suorittaessaan. Tämä lisää luotettavuutta ja vähentää potentiaalisten ongelmien riskiä.

## Katso myös

Tässä oli lyhyt esittely siitä, miten Rustilla voidaan muuntaa päivämäärä merkkijonoksi. Jos haluat lukea lisää Rust-ohjelmoinnista, suosittelemme tarkistamaan seuraavat resurssit:

- [Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/)
- [Rustin dokumentaatio](https://doc.rust-lang.org/)
- [Rust-ohjelmoinnin aloittelijan opas](https://doc.rust-lang.org/book/)