---
title:    "Rust: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarve laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä. Tämä voi olla esimerkiksi päivämääränäytön toteuttamiseksi kalenterisovelluksissa tai matkan varaukseen liittyvissä sovelluksissa. Rust-ohjelmointikielen avulla tämä laskeminen voidaan tehdä tehokkaasti ja luotettavasti.

## Kuinka tehdä

```Rust
// Esimerkki laskemisesta päivämäärästä 5 päivää tulevaisuuteen
use chrono::{NaiveDate, Duration};

let tanaan = NaiveDate::today();
let tulevaisuudessa = tanaan + Duration::days(5);

println!("5 päivän päästä on {}", tulevaisuudessa);
// Output: 5 päivän päästä on 2021-04-25
```

Esimerkissä käytetään Chrono-kirjastoa, joka tarjoaa monipuolisia työkaluja päivämäärän laskemiseen Rustissa. Perustana laskemiselle toimivat NaiveDate-rakenteet, jotka edustavat päivämäärää ilman aikavyöhykettä. Tulevaisuuden tai menneisyyden päivämääriä voidaan sitten laskea lisäämällä tai vähentämällä Duration-rakenteita tähän peruspäivämäärään.

## Syvempi sukellus

Vaikka NaiveDate-rakenteet soveltuvat hyvin yksinkertaisten päivämäärälaskelmien tekemiseen, on usein tarpeen käsitellä myös aikavyöhykkeitä sekä päivämäärän ja kellonajan yhdistelmiä. Tällöin on suositeltavaa tutustua myös toiseen Chrono-kirjaston tarjoamaan rakenteeseen, nimeltään DateTime. Tämä mahdollistaa tarkempien aikaselvitysten tekemisen ja mahdollistaa myös päivämäärän ja kellonajan muunnokset eri aikavyöhykkeiden välillä.

## Katso myös

- [Rustin virallinen opas aikojen ja päivämäärien käsittelyyn](https://doc.rust-lang.org/std/time/)
- [Chrono-kirjaston dokumentaatio](https://docs.rs/chrono/)