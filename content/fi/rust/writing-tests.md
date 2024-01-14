---
title:    "Rust: Ohjelmointitestien kirjoittaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamisen testaaminen on tärkeä osa ohjelmoinnin prosessia, jotta varmistetaan koodin toimivuus ja luotettavuus. Se myös auttaa ennaltaehkäisemään mahdollisia virheitä ja vähentämään debugging-aikaa.

## Miten

Yksi tapa testata Rust-koodia on käyttämällä standardikirjaston `assert_eq!` makroa. Tämä verrataan kahden arvon välillä ja jos ne eivät ole samat, testi epäonnistuu. Esimerkiksi:

```Rust
assert_eq!(5, 5); // tämä testi onnistuu
assert_eq!(5, 10); // tämä testi epäonnistuu
```

Tämä on hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, että tietyn funktion palauttama arvo on odotettu. Voit myös käyttää muita `assert` makroja, kuten `assert!` ja `assert_ne!`, riippuen tarpeistasi.

## Deep Dive

Testien kirjoittaminen Rustissa voi tuntua turhalta vaivalta, mutta se on tärkeä osa kehitysprosessia. Hyvä tapa aloittaa on kirjoittaa testi ensin ja sitten koodi, jotta voit varmistaa sen toimivuuden ennen kuin suoritat sen. Voit myös käyttää `#[test]` merkintää funktion edessä ilmoittamaan siitä, että se on testifunktio.

Rustilla on myös valmiita testejä, jotka automaattisesti suoritetaan jokaisen koodin muutoksen yhteydessä. Tämä auttaa varmistamaan, että mikään muutos ei riko olemassa olevia toimintoja.

## Katso myös

- [Rustin testauksen perusteet]() (eng.)
- [Testien kirjoittaminen Rustissa]() (eng.)
- [Serde-testing library]() (eng.)