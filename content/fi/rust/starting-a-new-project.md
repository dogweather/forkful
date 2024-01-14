---
title:                "Rust: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Rust on moderni ja tehokas ohjelmointikieli, joka on suunniteltu erityisesti tietokonejärjestelmien kehittämiseen. Sen avulla voit luoda nopeita, turvallisia ja luotettavia ohjelmia, jotka ovat helppoja ylläpitää ja laajentaa. Aloittaminen uuden projektin kanssa Rustilla on siis loistava tapa aloittaa uusi seikkailu ohjelmointimaailmassa!

## Kuinka aloittaa

Rustin asennus ja ympäristön valmistelu on helppoa, sillä se on täysin ilmainen ja avoimen lähdekoodin kieli. Voit ladata sen verkkosivustolta tai käyttää paketinhallintajärjestelmääsi, kuten Homebrew tai apt, asentaaksesi sen. Kun sinulla on Rust asennettuna, voit aloittaa uuden projektin seuraavilla komennoilla:

```Rust
$ cargo new projekti_nimi
$ cd projekti_nimi
```

Tämä luo uuden projektikansion, jossa on valmiiksi asennettu kaikki tarvittavat tiedostot. Voit aloittaa koodaamisen suoraan tiedostoon "src/main.rs", joka sisältää pienen "Hello World" -esimerkin. Kun olet valmis, voit suorittaa projektin komennolla:

```Rust
$ cargo run
```

Voit myös luoda uusia kansion sisällä olevia tiedostoja ja lisätä ne "src" -kansioon. Muista lisätä myös ne "Cargo.toml" -tiedostoon, joka toimii projektisi asetusohjelmana.

## Syvemmälle

Rust tarjoaa monia käteviä ominaisuuksia, kuten automaattinen muistinhallinta, rinnakkaisuuden tuki ja vahva tyyppijärjestelmä. Se on myös suunniteltu niin, että siitä on helppoa tehdä tehokkaita ja nopeita ohjelmia ilman turvallisuudesta tinkimistä. Jos haluat perehtyä syvemmin Rustin ominaisuuksiin ja mahdollisuuksiin, suosittelemme lukemaan virallista oppimateriaalia sekä kokeilemaan erilaisia projekteja ja harjoituksia.

## Katso myös

- [Rustin virallinen verkkosivusto](https://www.rust-lang.org)
- [Rustin oppimateriaali](https://doc.rust-lang.org/book)
- [Rust-ohjelmointiyhteisö Suomessa](https://forum.rust.fi)