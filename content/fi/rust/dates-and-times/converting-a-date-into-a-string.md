---
date: 2024-01-20 17:37:40.919848-07:00
description: "How to: Syv\xE4luotaus: Alun perin p\xE4iv\xE4m\xE4\xE4r\xE4n merkkijonomuotoon\
  \ muuttaminen ei kuulunut Rustin peruskirjastoon. K\xE4ytt\xE4j\xE4t joutuivat tukeutumaan\
  \ kolmannen\u2026"
lastmod: '2024-04-05T22:38:56.967711-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4luotaus: Alun perin p\xE4iv\xE4m\xE4\xE4r\xE4n merkkijonomuotoon\
  \ muuttaminen ei kuulunut Rustin peruskirjastoon. K\xE4ytt\xE4j\xE4t joutuivat tukeutumaan\
  \ kolmannen osapuolen kirjastoihin, kuten `chrono`. `chrono` on edelleen suosittu\
  \ vaihtoehto sen joustavuuden ja suorituskyvyn ansiosta. Vaihtoehtoja `chrono`-kirjastossa\
  \ on useita. Voit k\xE4ytt\xE4\xE4 `format`-metodia luodaksesi mukautettuja p\xE4\
  iv\xE4m\xE4\xE4r\xE4muotoja. Lis\xE4ksi `to_rfc2822` ja `to_rfc3339` kaltaiset funktiot\
  \ tarjoavat vakiomuotoisia merkkijonoja. Muuntamisen takana oleva toteutus perustuu\
  \ usein ajanhetken esitt\xE4miseen sekuntien tai nanosekuntien m\xE4\xE4r\xE4n\xE4\
  \ vuoden 1970 alusta (epoch time). T\xE4st\xE4 internaalisesta muodosta muodostetaan\
  \ sitten ihmislukukelpoinen merkkijono."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
Miten:

```Rust
use chrono::{DateTime, Utc, TimeZone};

fn main() {
    let now: DateTime<Utc> = Utc::now(); // Nykyhetki UTC-muodossa
    println!("Nykyhetki UTC: {}", now.to_string()); // Tulostetaan string-muodossa

    let custom_format = now.format("%Y-%m-%d %H:%M:%S").to_string(); // Määritetty formaatti
    println!("Mukautettu muoto: {}", custom_format); // Tulostetaan mukautetussa muodossa
}
```
Tuloste:
```
Nykyhetki UTC: 2023-04-05T14:30:10.501991Z
Mukautettu muoto: 2023-04-05 14:30:10
```

## Deep Dive
Syväluotaus:

Alun perin päivämäärän merkkijonomuotoon muuttaminen ei kuulunut Rustin peruskirjastoon. Käyttäjät joutuivat tukeutumaan kolmannen osapuolen kirjastoihin, kuten `chrono`. `chrono` on edelleen suosittu vaihtoehto sen joustavuuden ja suorituskyvyn ansiosta.

Vaihtoehtoja `chrono`-kirjastossa on useita. Voit käyttää `format`-metodia luodaksesi mukautettuja päivämäärämuotoja. Lisäksi `to_rfc2822` ja `to_rfc3339` kaltaiset funktiot tarjoavat vakiomuotoisia merkkijonoja.

Muuntamisen takana oleva toteutus perustuu usein ajanhetken esittämiseen sekuntien tai nanosekuntien määränä vuoden 1970 alusta (epoch time). Tästä internaalisesta muodosta muodostetaan sitten ihmislukukelpoinen merkkijono.

## See Also
Katso Myös:

- Chrono kirjaston dokumentaatio: https://docs.rs/chrono
- Rustin viralliset date ja time API:t: https://doc.rust-lang.org/std/time/index.html
- RFC 3339, päivämäärä- ja ajanhetkiformaatti: https://tools.ietf.org/html/rfc3339
