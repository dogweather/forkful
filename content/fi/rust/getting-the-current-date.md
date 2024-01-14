---
title:    "Rust: Nykyisen päivämäärän haku"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Haluatko lisätä nykyisen päivämäärän osaksi ohjelmaasi? Kuten monissa muissakin ohjelmointikielissä, Rustilla on myös helppo tapa saada nykyinen päivämäärä.

## Miten

Voit käyttää `chrono` kirjastoa saadaksesi nykyisen päivämäärän Rust-ohjelmassa. Ensimmäiseksi sinun täytyy lisätä kirjasto projektisi `Cargo.toml` tiedostoon:
````Rust
[dependencies]
chrono = "0.4.19"
````

Sitten voit luoda `Local` tyypin instanssin ja kutsua `now` metodia saadaksesi nykyisen päivämäärän:
````Rust
let current_date = chrono::Local::now();
````

Voit myös säätää millisekuntien tarkkuutta seuraavasti:
````Rust
let current_date = chrono::Local::now().to_rfc3339();
````

Ohessa on esimerkki koodi, joka tulostaa nykyisen päivämäärän konsoliin:
````Rust
use chrono::Local;

fn main() {
    let current_date = Local::now();
    println!("Today is {}", current_date.to_rfc3339());
}
````

Tässä on esimerkkitulostus:
````
Today is 2021-05-20T13:48:17.354798+03:00
````

## Syvennys

`chrono` kirjasto tarjoaa monia muita hyödyllisiä toimintoja päivämäärä- ja aikatyypeille. Voit esimerkiksi muuntaa aikoja toiseen aikavyöhykkeeseen tai laskostaa päivämääriä.

Kannattaa myös huomata, että päivämäärät ja ajat ovat immuuneja `DateTime`-tyypeissä, mikä tarkoittaa sitä, että niitä ei voi muuttaa suoraan, vaan näiden operaatioiden tulos on uusi instanssi. Tämän avulla voidaan varmistaa, että päivämäärä- ja aikatieto pysyy muuttumattomana.

## Katso myös

- `chrono` kirjaston dokumentaatio: https://docs.rs/chrono/0.4.19/chrono/ 
- Rust-oppimateriaali suomeksi: https://rust.weblog.cm 
- Rust-yhteisön virallinen sivusto: https://www.rust-lang.org/fi/