---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Rust: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen viittaa päivämäärän lisäämiseen tai vähentämiseen tietyn päivämäärän pohjalta. Ohjelmoijat tekevät tämän, jotta he voivat käsitellä ajankohdan erityisiä tehtäviä, kuten tapahtumien ajoitusta tai raporttien luomista.

## Näin tehdään:

Käytämme Rust-standardikirjastossa saatavilla olevaa `chrono`-kirjastoa. Lyhyesti, näin se menee:

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let two_weeks_from_now = now + Duration::weeks(2);
    let two_weeks_ago = now - Duration::weeks(2);

    println!("Nykyinen aika: {}", now);
    println!("Kahden viikon päästä: {}", two_weeks_from_now);
    println!("Kaksi viikkoa sitten: {}", two_weeks_ago);
}
```
Tässä esimerkkikoodissa luodaan ensin nykyinen päivämäärä ja aika, sitten lasketaan kaksi viikkoa tulevaisuuteen ja menneisyyteen käyttäen `Duration::weeks(2)`-toimintoa. Tulosteena saamme nykyisen ajankohdan, ajankohdan kahden viikon kuluttua ja ajankohdan kaksi viikkoa sitten.

## Syvällisempi tieto:

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on ollut ohjelmoinnin perustaito sen alkuajoista asti. Ennen vanhaan ohjelmoijat joutuivat tekemään paljon työtä tämän toteuttamiseksi, mutta nykyään useimmat ohjelmointikielet, kuten Rust, tarjoavat standardikirjastoissaan työkaluja ajan käsittelyyn.

Vaihtoehtoina `chrono`-kirjastolle voit käyttää myös `time`-kirjastoa, joka on toinen suosittu kirjasto ajan käsittelyyn Rust-yhteisössä.

Kun suoritat päivämäärän laskentaa tulevaisuuteen tai menneisyyteen, on tärkeää ottaa huomioon, että joissakin kuukausissa on eri määrä päiviä sekä karkausvuodet. Onneksi Rustin `chrono`-kirjasto ottaa nämä tekijät huomioon sinulle.

## Katso myös:

Lisätietoja Rustin päivämäärän ja ajan käsittelystä saat seuraavista lähteistä:

- Rustin virallinen dokumentaatio: https://doc.rust-lang.org/book/ 
- Chrono-kirjaston dokumentaatio: https://docs.rs/chrono/0.4.19/chrono/ 
- Gittubin Rust-ohjelmointikielen foorumin keskustelu aiheesta: https://users.rust-lang.org/t/how-do-i-add-days-to-a-date/1698