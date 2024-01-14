---
title:                "Rust: Tekstin etsiminen ja vaihtaminen"
simple_title:         "Tekstin etsiminen ja vaihtaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Niin kuin kaikissa ohjelmointikielissä, myös Rustin käyttäjien täytyy joskus suorittaa suuria muokkauksia tekstiin. Halutessasi esimerkiksi vaihtaa samalla kertaa kaikki numeraaliset merkit tekstistä, voisi olla hyödyllistä käyttää tekstin etsintä ja korvaaminen -toimintoa. Tämä auttaa säästämään aikaa ja vähentää mahdollisten virheiden määrää, mikä ilmenee käsin koodin muokkaamisesta.

## Miten

Tekstin etsintä ja korvaaminen Rustissa voidaan suorittaa käyttämällä match-avainsanoja ja regular expression -koodia kirjastossa "regex". Alla on esimerkki koodista, joka etsii tekstistä kaikki numerot ja korvaa ne tekstillä "Numero":

```Rust
use regex::Regex;

fn main() {
    let teksti = "Tämä on teksti, jossa on numeroita 123 ja 456.";

    let re = Regex::new(r"\d+").unwrap();
    let korvattu_teksti = re.replace_all(&teksti, "Numero");

    println!("{}", korvattu_teksti);
}
```

Tämä tulostaisi:

```
Tämä on teksti, jossa on numeroita Numero ja Numero.
```

## Syväsukellus

Regular expression eli säännöllinen lauseke on sarja merkkejä, jotka auttavat tunnistamaan ja etsimään tiettyjä kuvioita tekstistä. Rustin "regex"-kirjastossa on opas säännöllisiin lausekkeisiin, joka auttaa ymmärtämään lisää tästä aiheesta.

On myös mahdollista suorittaa monimutkaisempia etsintöjä ja korvaamisia, kuten eri merkkiketjujen yhdistelmiä tai sanojen vaihtamista. Tärkeintä on ymmärtää säännöllisten lausekkeiden ja match-avainsanojen perustoiminnallisuudet, ja sen jälkeen voi kokeilla erilaisia tapoja löytää ja korvata tekstiä.

## Katso myös

- Rustin virallinen oppikirja: [https://rust-lang.github.io/book/](https://rust-lang.github.io/book/)
- "regex"-kirjaston dokumentaatio: [https://docs.rs/regex/1.4.3/regex/](https://docs.rs/regex/1.4.3/regex/)
- Säännölliset lausekkeet: [https://en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)