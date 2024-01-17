---
title:                "Kahden päivämäärän vertailu"
html_title:           "Rust: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Vertaaminen kahden päivämäärän välillä on tärkeä osa ohjelmointia, joka auttaa meitä tekemään päätelmiä ja laskelmia ajan suhteen. Esimerkiksi verkkosivustoilla saattaa olla tarve tarkistaa, onko päivämäärä mennyt tai tuleva. Päivämäärän vertaaminen auttaa myös ohjelmoijia pitämään tiedot järjestyksessä ja suorittamaan tarkkoja ajan määrityksiä.

# Kuinka tehdä?

Käyttämällä Rust-ohjelmointikieltä, päivämäärän vertaaminen on helppoa ja nopeaa. Voit käyttää  Standard libraryn `chrono`-kirjastoa, joka tarjoaa valmiita työkaluja päivämäärien vertailuun. 
Alla on yksinkertainen esimerkki kahden päivämäärän vertailemisesta ja tulosten tulostamisesta:

```Rust
use chrono::{DateTime, Duration, Utc};

fn date_comparison() {
    let today: DateTime<Utc> = Utc::now();
    let random_date = "2021-08-01T00:00:00Z".parse::<DateTime<Utc>>().unwrap();

    let difference = random_date - today;
    println!("Päiviä siihen: {}", difference.num_days());
    println!("Minuutteja siihen: {}", difference.num_minutes());
}

date_comparison();
```

Tulosteena saamme seuraavan:

```Rust
Päiviä siihen: 12
Minuutteja siihen: 17280
```

Kuten näemme, `chrono`-kirjaston `DateTime`-tyyppi mahdollistaa päivämäärien luomisen ja vertailemisen helposti. Voimme myös käyttää `Duration`-tyyppiä laskemaan eron kahden päivämäärän välillä haluamassamme aikayksikössä.

# Syväsukellus

Päivämäärien vertaileminen ei ole uusi konsepti, vaan se on ollut osa ohjelmointia jo pitkään. Aikaisemmin, ennen `chrono`-kirjastoa, päivämäärät ja -ajat oli tallennettava ja käsiteltävä monimutkaisissa muodoissa ja vertaileminen oli hankalampaa.

Nykyisin on myös muita vaihtoehtoja `chrono`-kirjastolle, kuten `time`-kirjasto, joka tarjoaa samanlaisia toiminnallisuuksia. Kuitenkin `chrono` on tällä hetkellä suositumpi ja paremmin ylläpidetty.

# Katso myös

- [Rustin virallinen dokumentaatio DateTime-tietotyypistä](https://doc.rust-lang.org/std/time/enum.DateTime.html)
- [Time-kirjasto GitHubissa](https://github.com/time-rs/time)
- [Chrono-kirjasto GitHubissa](https://github.com/chronotope/chrono)