---
title:    "Rust: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi muuntaa päivämäärä merkkijonoksi?

Monissa ohjelmointiprojekteissa saattaa olla tarve muuntaa päivämäärä- ja aikatieto merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi tietokannoissa tai tiedostojen nimeämisessä. Rust-kieli tarjoaa tehokkaan ja turvallisen tavan suorittaa tämä muunnos, mikä tekee siitä hyvän vaihtoehdon tähän tarpeeseen.

## Miten se tehdään?

Käytännössä päivämäärän muuntaminen merkkijonoksi Rustilla tapahtuu yhdellä rivillä koodia. Tämä tapahtuu DateTime-kirjastosta löytyvän "to_string" -metodin avulla. Alla on yksinkertainen esimerkki, jossa otetaan nykyinen päivämäärä ja aika ja muunnetaan se merkkijonoksi:

```Rust
use chrono::prelude::*;

let now = Local::now();
let date_string = now.to_string();
println!("{}", date_string);
```

Tämän yksinkertaisen esimerkin tuloste olisi jotain tällaista:

```
2021-12-01 14:30:00.000000000 +0300
```

Tämä antaa päivämäärän ja ajan lisäksi myös aikavyöhykkeen sekä nanosekunnit.

## Syvemmälle

DateTime-kirjastossa on muitakin vaihtoehtoja muuntaa päivämäärä merkkijonoksi. Voit esimerkiksi määrittää halutunmuodon, johon päivämäärä muunnetaan, käyttämällä "format" -metodia sen sijaan, että käyttäisit "to_string" -metodia. Alla on esimerkki tästä:

```Rust
use chrono::prelude::*;
use chrono::format::strftime::StrftimeItems;

let now = Local::now();
let format = StrftimeItems::new("%Y/%m/%d %H:%M:%S").to_string();
let date_string = now.format(format);
println!("{}", date_string);
```

Tämän esimerkin tuloste olisi:

```
2021/12/01 14:30:00
```

Voit myös käyttää DateTime-kirjastoa kääntämään merkkijono takaisin päivämääräksi ja ajaksi, jos tarvitset sitä jatkossa.

## Katso myös

- [Rust DateTime-kirjasto](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)
- [Päivämäärän muuntaminen merkkijonoksi Rustilla Stack Overflow-sivustolla](https://stackoverflow.com/questions/28015568/how-do-i-convert-a-chrono-datetime-to-a-string)
- [Rust-ohjeet päivämäärän muuntamiseen merkkijonoksi](https://users.rust-lang.org/t/how-to-format-a-date/2346)