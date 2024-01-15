---
title:                "Aloittaminen uudesta projektista"
html_title:           "Rust: Aloittaminen uudesta projektista"
simple_title:         "Aloittaminen uudesta projektista"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Ajattele hetki kaikkia niitä upeita projekteja, joita voit luoda käyttäen Rust-ohjelmointikieltä. Ne voivat olla mitä tahansa luotettavista ja nopeista backend-järjestelmistä mielenkiintoisiin etätietokanta-sovelluksiin. Rust tarjoaa vahvan ja modernin ohjelmointikokemuksen ja se onkin monien kehittäjien valinta uusien projektien aloittamiseen.

## Kuinka aloittaa

Rustin asentaminen on yksinkertaista ja nopeaa. Voit mennä osoitteeseen [rustup.rs](https://rustup.rs/), josta löydät asennusohjeet kaikille yleisimmille käyttöjärjestelmille. Kun asennus on valmis, voit luoda uuden projektin käyttäen seuraavaa komentoa terminaalissa:

```
Rust uusi minun_projekti
```

Tämä luo uuden kansiorakenteen, jossa on kaikki tarvittavat tiedostot uuden projektin aloittamiseen. Nyt voit siirtyä projektin juurihakemistoon ja aloittaa koodaamisen!

Tässä esimerkissä käytetään [Cargo](https://doc.rust-lang.org/cargo/) -työkalua, joka on Rustin virallinen paketinhallintajärjestelmä ja rakennusautomaattori. Voit lisätä uusia riippuvuuksia projektiisi, kääntää ja suorittaa koodisi kätevästi käyttämällä Cargoa.

```
cd minun_projekti
cargo run
```

Näin koodisi kääntyy ja suoritetaan. Voit myös luoda suoritettavan tiedoston käyttämällä seuraavaa komentoa:

```
cargo build
```

Ja voit suorittaa sen käyttämällä komentoa:

```
./target/debug/minun_projekti
```

## Syvenny

Kun aloitat uuden projektin Rustilla, on tärkeää harkita projektin rakennetta ja arkkitehtuuria. Voit luoda moduleja, jotka jakavat koodin loogisiin osiin, ja käyttää Rustin vahvaa tyyppijärjestelmää varmistaaksesi, että koodi on turvallinen ja luotettava.

Rustilla on myös käytettävissä valtava määrä kolmannen osapuolen kirjastoja, jotka voivat auttaa sinua toteuttamaan erilaisia toiminnallisuuksia projektissasi. Voit lukea lisää [crates.io](https://crates.io/) -sivustolta, jossa on saatavilla yli 60 000 Rust-kirjastoa.

## Katso myös

- [Rust-oppaan aloittaminen](https://doc.rust-lang.org/book/ch01-00-getting-started.html)
- [Käytetyimmät Rust-paketit](https://crates.io/categories)
- [Rustin yhteisö ja tuki](https://www.rust-lang.org/community)