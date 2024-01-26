---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:45:36.042858-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kompleksiluvuilla on reaaliosa ja imaginaariosa, ja ne ovat keskeisiä monilla aloilla, kuten insinööritieteissä, fysiikassa ja tietokonegrafiikassa. Ohjelmoijat käyttävät niitä ratkaistakseen yhtälöitä, joita tavalliset reaaliluvut eivät pysty käsittelemään.

## Kuinka:
Rust ei suoraan tue kompleksilukuja, mutta kirjastot, kuten `num-complex`, ovat apunasi. Näin voit käyttää sitä:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let summa = a + b;
    let tulo = a * b;

    println!("Summa: {}", summa); // Summa: 3 - 1i
    println!("Tulo: {}", tulo); // Tulo: 14 - 5i
}
```
Sinun täytyy lisätä `num_complex` `Cargo.toml`-tiedostoosi, jotta tämä taika tapahtuu.

## Syväsukellus
Kompleksiluvut keksittiin 1500-luvulla, mutta ne todella lähtivät lentoon 1700-luvulla, kun matemaatikot, kuten Euler, alkoivat leikitellä niillä.

Natiivien kompleksilukutoimintojen puuttuessa kielet, kuten Rust, nojaavat kolmannen osapuolen kirjastoihin. `num-complex` on yksi tällainen kirjasto ja osa `num`-kirjastokokoelmaa, jonka tavoitteena on tarjota numeerisia tyyppejä ja piirteitä Rustille.

On mainitsemisen arvoista, että jotkin kielet (kuten Python) tukevat kompleksilukuja suoraan, kun taas toiset (kuten C++, `<complex>`-otsikon kanssa) tarjoavat ne osana standardikirjastoa. Rustissa päätös pitää standardikirjasto pienenä tarkoittaa, että usein käännymme yhteisön luomien kirjastojen puoleen lisätoiminnallisuuden saamiseksi.

## Katso Myös
- [Rust-kirja](https://doc.rust-lang.org/book/): Lisätietoja Rustista ja ulkoisten kirjastojen käytöstä.
- [Kompleksiluku Wikipedia](https://en.wikipedia.org/wiki/Complex_number): Syvempi ymmärrys kompleksiluvuista.