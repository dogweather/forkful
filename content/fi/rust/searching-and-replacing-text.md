---
title:    "Rust: Tekstin etsiminen ja korvaaminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi hakea ja korvata tekstiä?

Hakea ja korvata tekstiä on olennainen osa koodauksen ja ohjelmoinnin maailmaa. Se voi auttaa säästämään aikaa ja vaivaa manuaalisesti muokkaamalla tekstejä ja koodinpätkiä.

## Miten se tehdään?

Hakeminen ja korvaaminen tekstissä on melko yksinkertaista Rust-ohjelmointikielen avulla. Seuraa alla olevia esimerkkejä ja tulostetta käyttäen ```Rust ... ``` koodilohkoja.

```
fn main() {
    let teksti = "Hei kaikille!";
    let uusi_teksti = teksti.replace("Hei", "Moikka");
    println!("{}", uusi_teksti);
}
```
Tuloste:
```
Moikka kaikille!
```
Tässä esimerkissä olemme luoneet muuttujan nimeltä "teksti" ja antaneet sille arvon "Hei kaikille!". Sitten olemme käyttäneet ```replace()``` -funktiota korvataksemme "Hei" sanan "Moikka" sanaan ja tallentaneet sen uuteen muuttujaan nimeltä "uusi_teksti". Lopuksi tulostamme uuden tekstin ```println!``` -funktiolla.

Voit myös käyttää regex -regexpressejä hakemaan ja korvaamaan tekstiä. Esimerkiksi:

```
use regex::Regex;

fn main() {
    let teksti = "Tämä on tekstiä.";
    let korvaus = Regex::new("text").unwrap();
    let uusi_teksti = korvaus.replace_all(teksti, "suurempi teksti");
    println!("{}", uusi_teksti);
}
```
Tuloste:
```
Tämä on suurempi tekstiä.
```

## Syvemmälle tekstien haun ja korvauksen pariin

Rust tarjoaa monia vaihtoehtoja tekstien hakemiseen ja korvaamiseen, kuten erilaisia ```replace()``` -funktioita ja regex -regexpressejä. Voit myös yhdistellä näitä vaihtoehtoja luodaksesi tehokkaampia ja monimutkaisempia koodinpätkiä.

Yksi tärkeä asia huomata on, että tekstien hakeminen ja korvaaminen on yleensä case-sensitive eli se huomioi tekstissä käytetyt isot ja pienet kirjaimet. Voit kuitenkin käyttää ```replace_ignore_case()``` -funktiota, jotta haku ja korvaus toimisi zakena.

## Katso myös

- [Rustin dokumentaatio tekstien hakemiseen ja korvaamiseen](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Regex - regexpresseihin tutustuminen](https://doc.rust-lang.org/regex/regex/index.html)
- [Lisää Rustin perusteita](https://www.rust-lang.org/learn)