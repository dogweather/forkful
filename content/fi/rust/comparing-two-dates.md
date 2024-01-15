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

## Miksi

Haluatko tietää, miten kaksi päivämäärää vertaillaan? Ehkä olet juuri aloittanut Rust-ohjelmoinnin ja haluat oppia tarkemmin tästä aiheesta. Tämä artikkeli kertoo sinulle kaiken, mitä tarvitset tietää vertaillaaksesi kahta päivämäärää Rust-ohjelmassa.

## Miten

Vertaaminen Rustissa on helppoa, koska kielessä on valmiiksi sisäänrakennettu tietotyyppi day, joka mahdollistaa päivämäärien käsittelyn. Voit käyttää tämän tietotyypin metodeja vertaillaksesi päivämääriä keskenään.

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let today = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
let yesterday = today - 86400; // 86400 sekuntia on yksi päivä

if today > yesterday {
    println!("Tänään on uudempi päivä kuin eilen.");
} else if today < yesterday {
    println!("Eilen oli uudempi päivä kuin tänään.");
} else {
    println!("Päivät ovat yhtä suuret.");
}
```

Tämä koodi luo kaksi päivämäärää: tänään ja eilen. Sitten se vertailee niitä ja tulostaa viestin sen mukaan, kumpi päivä on uudempi. Huomaa, että voit myös vertailla päivämääriä relaatio-operaattoreilla, kuten `>`, `<` ja `==`.

## Syvempi sukellus

Kaksi päivämäärää vertaillaan yleensä tarkastelemalla niiden aikatietoja. Rustin day-tietotyyppi tallentaa päivämäärän aikaleiman sekunteina UNIX-aikaleimaan nähden. Siksi voit laskea näiden aikaleimojen erotuksen ja nähdä, kumpi päivämäärä on edellisempi.

On myös tärkeää huomata, että tietotyypin day käyttö voi olla hieman haasteellista, jos olet tottunut käyttämään päivämääriä muissa kielissä, kuten JavaScript. Esimerkiksi päivämäärän muotoilu ja erilaisten aikavyöhykkeiden huomioiminen voi aiheuttaa vaikeuksia. Siksi on suositeltavaa tarkistaa dokumentaatiosta tarkemmat tiedot ennen päivämäärien vertailua.

## Katso myös

- [Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/)
- [Rustin päivämäärä- ja kellotoimintojen dokumentaatio](https://doc.rust-lang.org/std/time/index.html)
- [Rustin päivämäärät, aikavyöhykkeet ja kellonajat - opas (englanniksi)](https://web.archive.org/web/20170312162801/https://lmg.io/blog/data/2016/11/28/rusts-date-time-and-timezone-lmg.html)