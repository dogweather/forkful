---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän muuntaminen merkkijonoksi on prosessi, jossa päivämäärä esitetään tietokoneelle luettavassa tekstissä. Ohjelmoijat tekevät tämän usein päivämäärätietojen visualisointiin tai lokiin.

## Kuinka tehdä:

Rustissa päivämäärän hieman muuttamiseksi tekstimuotoon käytetään `chrono` kirjastoa ja `NaiveDate` tyyppiä. Tässä on perusesimerkki:

```Rust
use chrono::{NaiveDate, Datelike, Weekday};

fn main() {
    let date = NaiveDate::from_ymd(2016, 7, 8);
    println!("{}", date);
}
```

Kun suoritat tämän koodin, tulostus on: `2016-07-08`.

## Syvä Sukellus:

Rustissa päivämäärän ja ajan hallintaan suositellaan `chrono`-kirjastoa, joka esiteltiin Rustin versiossa 0.4.0. Se tarjoaa rikkaan joukon ominaisuuksia päivämäärän ja ajan käsittelyyn. Jos sinun ei tarvitse työskennellä aikavyöhykkeiden kanssa, `NaiveDate` on yksinkertainen ja suoraviivainen ratkaisu.

Vaihtoehtoisia lähestymistapoja ovat `time`-kirjasto tai käyttäjän määrittämät ratkaisut, jotka käyttävät perinteisiä unix-aikaleimoja. Kuitenkin, `chrono` tarjoaa parhaan tasapainon käytettävyyden ja tehokkuuden välillä.

Päivämäärän muuttamisen toteutustiedot riippuvat tarpeistasi. `chrono`-kirjasto tukee useita muotoja, joten sinun tulee valita sopivin.

## Katso Myös:

Lisätietoja voit löytää seuraavista lähteistä:
* Chrono-kirjasto: https://docs.rs/chrono/0.4.19/chrono/
* Rustin virallinen dokumentaatio: https://doc.rust-lang.org/std/
* StackOverflow keskustelut aiheesta: https://stackoverflow.com/questions/tagged/rust+chrono