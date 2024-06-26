---
date: 2024-01-20 17:31:48.753427-07:00
description: "How to: - Kuinka Tehd\xE4\xE4n: Rustissa p\xE4iv\xE4m\xE4\xE4r\xE4n\
  \ laskenta tulevaisuuteen tai menneisyyteen hoituu `chrono`-kirjaston avulla. Alla\
  \ esimerkkej\xE4."
lastmod: '2024-04-05T22:38:56.969727-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka Tehd\xE4\xE4n: Rustissa p\xE4iv\xE4m\xE4\xE4r\xE4n laskenta tulevaisuuteen\
  \ tai menneisyyteen hoituu `chrono`-kirjaston avulla. Alla esimerkkej\xE4."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

## How to: - Kuinka Tehdään:
Rustissa päivämäärän laskenta tulevaisuuteen tai menneisyyteen hoituu `chrono`-kirjaston avulla. Alla esimerkkejä.

```rust
use chrono::{Duration, Utc};

fn main() {
    let nykyhetki = Utc::now();
    println!("Nykyhetki: {}", nykyhetki);

    let viikon_paasta = nykyhetki + Duration::days(7);
    println!("Viikon päästä: {}", viikon_paasta);

    let viikko_sitten = nykyhetki - Duration::days(7);
    println!("Viikko sitten: {}", viikko_sitten);
}
```

Esimerkin tulostus:

```
Nykyhetki: 2023-04-07T12:34:56Z
Viikon päästä: 2023-04-14T12:34:56Z
Viikko sitten: 2023-03-31T12:34:56Z
```

## Deep Dive - Syväluoto
`chrono` on Rust-kirjasto, joka käsittelee aikaa. Se on julkaistu vuonna 2014. `chrono` on suosittu, koska se on turvallinen ja helppokäyttöinen. Vaihtoehtoisesti Rust standardikirjasto tarjoaa perustoiminnallisuutta, mutta se ei ole yhtä voimakas eikä joustava.

Tarkastellaan esimerkkiä. `Utc::now()` palauttaa nykyhetken UTC-aikavyöhykkeellä. `Duration::days` luo keston päiviä. Laskennat yhteen- ja vähennyslaskujen avulla ovat suoraviivaisia.

Rustissa ajan ja päivämäärän käsittely on tyyppiturvallista. Tämä tarkoittaa, että virheellisiä operaatioita, kuten sekuntien lisäämistä päivämäärään, ei voi tehdä huomaamatta. Se auttaa välttämään bugit ja tekee koodista luotettavamman.

## See Also - Katso Myös
- [Rust-dokumentaatio ajan käsittelyyn](https://doc.rust-lang.org/std/time/)
- [UTC ja aikavyöhykkeet](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
