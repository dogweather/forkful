---
title:                "Päivämäärän erottaminen merkkijonosta"
html_title:           "Rust: Päivämäärän erottaminen merkkijonosta"
simple_title:         "Päivämäärän erottaminen merkkijonosta"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän muuttamista tietokoneen ymmärtämään muotoon. Ohjelmoijat tekevät tätä helpottaakseen päivämäärien käsittelyä ja muokkaamista sovelluksissa.

## Kuinka:
```Rust
let date_string = "13.9.2021";
let date = date_string.parse::<DateTime<Utc>>().unwrap();
println!("{}", date.format("%d %B, %Y"));
```

Tässä esimerkissä muutamme "13.9.2021" merkkijonon tietokoneen ymmärtämään DateTime-muotoon ja tulostamme päivämäärän muodossa "13 syyskuuta, 2021".

## Syvempi sukellus:
Päivämäärän parsiminen merkkijonosta on ollut haasteellista ohjelmointitaikojen alusta asti, sillä eri maissa ja kulttuureissa käytetään erilaisia päivämäärämuotoja. Useimmiten parsiminen tehdään apumoduulien avulla, kuten Rustin Chrono-moduulilla. Myös koodien optimointi ja virheiden hallinta ovat tärkeitä osia päivämäärän parsimisessa.

## Katso myös:
- [Rustin virallinen dokumentaatio DateTime-muodosta ja sen käytöstä](https://doc.rust-lang.org/std/datetime/struct.DateTime.html)
- [Rust-kirjastoja päivämääränmuutoksiin](https://arewelearningyet.com/datetime-library/)
- [Päivämäärän parsimisen vaihtoehtoiset lähestymistavat](https://distrans.org/2018/08/23/swallow-waterfall/)