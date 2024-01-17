---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
html_title:           "Rust: Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen tietokoneohjelmoinnissa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Rustilla tulevaisuuden tai menneen päivämäärän laskeminen

## Mitä & Miksi?

Tulevaisuuden tai menneen päivämäärän laskeminen on ohjelmisto-osa, jolla pystytään päivämäärän laskemiseen tietyn ajanjakson päässä nykyisestä päivämäärästä. Tätä tehdään usein liiketoiminnallisissa sovelluksissa, kuten laskutuksessa tai kalenterisovelluksissa.

## Kuinka:

Seuraavassa käytetään esimerkkinä Rustin Date Crate -kirjastoa laskemaan tulevaisuuden päivämäärä.

```Rust
use chrono::{Utc, Duration};

let current_date = Utc::today();
let days_to_add = Duration::days(7);
let future_date = current_date + days_to_add;

println!("Nykyinen päivämäärä: {}", current_date);
println!("7 päivää lisää: {}", future_date);
```

Tulostus:

```
Nykyinen päivämäärä: 2021-10-04
7 päivää lisää: 2021-10-11
```

## Syvempi sukellus:

Historiallisessa kontekstissa päivämäärän laskeminen ei aina ollut helppoa, ja monissa vanhemmissa kielissä se vaati melko paljon koodia. Onneksi nykyaikaiset kielet, kuten Rust, tarjoavat kirjastoja, jotka helpottavat päivämäärän laskemista.

On myös muita tapoja laskea päivämäärä, kuten käyttämällä Unix-aikaleimoja tai date-komentoa komentoriviltä. Näitä tapoja voi olla hyödyllistä käyttää erilaisissa skenaarioissa.

Päivämäärän laskeminen tapahtuu yleensä millisekunneissa, ja tarkkuutta vaikutetaan päivämääräkirjaston avulla. On tärkeää varmistaa, että päivämääräkirjasto on asennettu ja päivitetty, jotta laskenta on tarkkaa.

## Katso myös:

- [Chrono Crate - Rustin päivämäärä- ja aikanenälkymä](https://docs.rs/chrono/0.4.19/chrono/)
- [Rustin standardikirjaston Date Crate -näkymä](https://doc.rust-lang.org/std/time/struct.SystemTime.html)

Kiitos, kun luit! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään tarkemmin miten tulevaisuuden tai menneen päivämäärän laskeminen tapahtuu Rustilla.