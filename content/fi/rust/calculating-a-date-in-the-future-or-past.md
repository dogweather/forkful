---
title:                "Rust: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Rust on suosittu ohjelmointikieli monipuolisen ja luotettavan tyyppijärjestelmänsä ansiosta. Yksi sen voimista on myös sen kyky käsitellä aikaa ja päivämääriä tehokkaasti. Tässä blogikirjoituksessa tutustumme siihen, miten voimme laskea tulevan tai menneen päivämäärän Rustin avulla.

## Kuinka tehdä

Laskeminen tulevan tai menneen päivämäärän Rustilla on helppoa käyttämällä "chrono" kirjastoa. Ensiksi, tuo kirjasto projektiisi riippuvuuksien luetteloon ja käytä sen Date-pakkausta määrittämään haluttu päivämäärä:

```Rust
use chrono::{Date, Duration, Local, NaiveDate};
let current_date: Date<Local> = Local::today();
let future_date = current_date + Duration::days(7);
let past_date = current_date - Duration::weeks(2);
```

Ensimmäinen rivi tuo "chrono" kirjaston projektiin ja luo uuden päivämäärän tyyppiä "Local". Tällä hetkellä olevan päivämäärän saa käyttämällä "today()" metodia.

Seuraavat kaksi riviä laskevat tulevan ja menneen päivämäärän käyttämällä "+" ja "-" operaattoreita sekä "Duration" tyyppiä. Tässä esimerkissä lisätään 7 päivää ja vähennetään 2 viikkoa nykyisestä päivämäärästä.

Lopuksi, voit tulostaa päivämäärät käyttämällä "println!" makroa:

```Rust
println!("Tuleva päivämäärä: {}", future_date);
println!("Menneet päivämäärät: {}", past_date);
```

Kun ajat koodin, saat seuraavan tulosteen:

```
Tuleva päivämäärä: 2021-04-20
Menneet päivämäärät: 2021-04-06
```

## Syväsukellus

"chrono" kirjasto tarjoaa laajan valikoiman toimintoja ja metodeja päivämäärien käsittelyyn. Voit esimerkiksi laskea päivien, viikkojen ja kuukausien määrän kahden päivämäärän välillä, muuntaa päivämäärän eri aikavyöhykkeelle tai formatoida päivämäärän halutunlaiseksi.

Lisäksi, "chrono" kirjaston avulla voit myös laskea aikaleimoja ja kellonaikoja. Se tarjoaa myös tuki tuleville aikavyöhykkeiden muutoksille, mikä tekee siitä erittäin luotettavan.

## Katso myös
- Chrono dokumentaatio: https://docs.rs/chrono/0.4.19/chrono/ 
- Rust dokumentaatio: https://doc.rust-lang.org/std/time/index.html 
- "Date and Time Handling in Rust" Medium-artikkeli: https://medium.com/@rickyattds/date-and-time-handling-in-rust-be263f819b96