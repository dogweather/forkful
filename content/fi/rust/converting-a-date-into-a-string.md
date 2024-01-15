---
title:                "Päiväyksen muuntaminen merkkijonoksi"
html_title:           "Rust: Päiväyksen muuntaminen merkkijonoksi"
simple_title:         "Päiväyksen muuntaminen merkkijonoksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoijana sinun täytyy muuntaa päivämäärä tekstiksi. Tämä voi olla tarpeellista esimerkiksi silloin, kun haluat tulostaa päivämäärän käyttäjälle tai tallentaa sen tietokantaan.

## Miten

Rust tarjoaa erilaisia tapoja muuntaa päivämäärä tekstiksi. Käymme tässä läpi kaksi yleisintä tapaa, `strftime` ja `to_string`.

```
Rust ## Miksi

Joskus ohjelmoijana sinun täytyy muuntaa päivämäärä tekstiksi. Tämä voi olla tarpeellista esimerkiksi silloin, kun haluat tulostaa päivämäärän käyttäjälle tai tallentaa sen tietokantaan.

## Miten

Rust tarjoaa erilaisia tapoja muuntaa päivämäärä tekstiksi. Käymme tässä läpi kaksi yleisintä tapaa, `strftime` ja `to_string`.

```
use chrono::format::strftime::StrftimeItems;
use chrono::Date;
use chrono::Utc;

fn main() {
    let date = Utc::today();
    let items = StrftimeItems::new("%A, %B %d, %Y");
    let formatted = date.format_with_items(items);
    println!("{}", formatted);
}
```

Tämä koodi tulostaa päivämäärän seuraavassa muodossa: "keskiviikko, tammikuu 27, 2021".

Voit myös käyttää `to_string`-metodia muuntaaksesi päivämäärän suoraan tekstiksi ilman erillisiä muotoiluja:

```
use chrono::Date;
use chrono::Utc;

fn main() {
    let date = Utc::today();
    let formatted = date.to_string();
    println!("{}", formatted);
}
```

Tämä koodi tulostaa päivämäärän seuraavassa muodossa: "2021-01-27".

## Syvemmälle

Chrono-kirjasto, joka tarjoaa päivämäärämuunnostyökalut Rust-ohjelmointikieleen, perustuu osittain Glossary-kirjastoon. Glossary tarjoaa monia erilaisia muotoiluja, jotka voit antaa `strftime`-metodille muuntaaksesi päivämäärän tekstiksi haluamallasi tavalla.

Voit myös käyttää erilaisia pikanäppäimiä, kuten `%B` tai `%Y`, jotka muodostavat automaattisesti osan päivämäärästä (esimerkiksi kuukauden nimen tai vuosiluvun).

## Katso myös

- Chrono-kirjasto: https://docs.rs/chrono/latest/chrono/
- Glossary-kirjasto: https://docs.rs/glossary/latest/glossary/