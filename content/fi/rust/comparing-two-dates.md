---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Vertaamme kahta päivämäärää, kun haluamme selvittää niiden suhteelliset sijainnit ajassa. Ohjelmoijat käyttävät tätä toimintoa tarpeesta arvioida aikavälejä, järjestää tietoja tai suorittaa erityisiä tehtäviä päivämäärän perusteella.

## Kuinka Tehdä:
Tässä on yksinkertainen esimerkki kahden päivämäärän vertailusta Rust-koodissa:

```Rust
use std::cmp::Ordering;
use chrono::NaiveDate;

fn main() {
    let date1 = NaiveDate::from_ymd(2020, 7, 5);
    let date2 = NaiveDate::from_ymd(2021, 2, 3);

    match date1.cmp(&date2) {
        Ordering::Less => println!("Date1 on ennen Date2"),
        Ordering::Greater => println!("Date1 on jälkeen Date2"),
        Ordering::Equal => println!("Date1 ja Date2 ovat samat"),
    }
}
```

Koodi tulostaa:

```
Date1 on ennen Date2
```

## Syvällinen Tieto:
Historiallisesti päivämäärien vertaaminen on ollut osa ohjelmointia lähes alusta asti, kun tietokoneliitteiset järjestelmät alkoivat tarvita tapoja käsitellä ja arvioida aikaa.

Vaihtoehtoja päivämäärien vertailuun Rustissa on monia. Esimerkiksi voit käyttää `PartialEq` ja `PartialOrd` lisäämään tarkkuutta ja monimutkaisuutta.

Rustissa päivämäärien vertaaminen perustuu sisäisesti nanosekuntien lukumäärään, joka on kulunut tietystä referenssipisteestä (yleensä Unix Epoch, eli 1. tammikuuta 1970). Tämä mahdollistaa tarkan ja luotettavan vertailun.

## Katso Myös:
Muista tutkia nämä linkit lisätietoja varten:

1. Chrono Crate Dokumentaatio: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
2. Rust Ohjelmointikielen Dokumentaatio: [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
3. Rust By Example: DateTime: [https://doc.rust-lang.org/stable/rust-by-example/std_misc/datetime.html](https://doc.rust-lang.org/stable/rust-by-example/std_misc/datetime.html)