---
title:    "Rust: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmat vaativat päivämäärän esittämistä merkkijonona, esimerkiksi tulostuksen yhteydessä. Rustin tarjoama DateTime-kirjasto tarjoaa hyödyllisiä toimintoja päivämäärän ja ajan manipulointiin, mukaan lukien päivämäärän muuntaminen merkkijonoksi. Seuraavassa kerromme, miten tämä onnistuu.

## Kuinka

Converting a date into a string -toimintoa varten tarvitsemme ensin päivämäärämuuttujan, johon tallennetaan haluttu päivämäärä ja aika. Tämän jälkeen voimme käyttää DateTime-kirjastosta löytyviä toimintoja päivämäärän muuntamiseen merkkijonoksi. Alla on esimerkkikoodia, jossa haluamme tulostaa päivämäärän muodossa "päivä.kuukausi.vuosi".

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use chrono::prelude::*;

// Luodaan päivämäärämuuttuja
let dt = Utc::now();

// Muunnetaan päivämäärä merkkijonoksi
let date_str = dt.format("%d.%m.%Y").to_string();

// Tulostetaan kauniilla
println!("Haluttu päivämäärä on: {}", date_str);
```

Esimerkkitulostus:

```
Haluttu päivämäärä on: 31.12.2021
```

## Syväkatsaus

DateTime-kirjastosta löytyy useita erilaisia muotoilutoimintoja, joilla päivämäärä voidaan muuntaa haluttuun merkkijonon muotoon. Voit kokeilla esimerkissä käytetyn "%d.%m.%Y" sijasta esimerkiksi "%A, %B %e, %Y", jolloin saat tulokseksi viikonpäivän, kuukauden nimen, kuukauden päivän ja vuoden.

See Also (Katso myös):
- [DateTime kirjaston dokumentaatio](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)
- [Rustin DateTime esimerkit](https://github.com/chronotope/chrono/blob/master/examples/clock.rs)