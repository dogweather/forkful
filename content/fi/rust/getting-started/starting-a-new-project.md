---
date: 2024-01-20 18:04:56.012937-07:00
description: "Aloitamme uuden projektin luomalla puhtaan ty\xF6tilan, joka sis\xE4\
  lt\xE4\xE4 kaikki tarvittavat tiedostot ja hakemistorakenteet. Koodareina teemme\
  \ t\xE4m\xE4n, koska se\u2026"
lastmod: '2024-03-11T00:14:30.291658-06:00'
model: gpt-4-1106-preview
summary: "Aloitamme uuden projektin luomalla puhtaan ty\xF6tilan, joka sis\xE4lt\xE4\
  \xE4 kaikki tarvittavat tiedostot ja hakemistorakenteet. Koodareina teemme t\xE4\
  m\xE4n, koska se\u2026"
title: Uuden projektin aloittaminen
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Aloitamme uuden projektin luomalla puhtaan työtilan, joka sisältää kaikki tarvittavat tiedostot ja hakemistorakenteet. Koodareina teemme tämän, koska se nopeuttaa kehitystä ja pitää koodimme organisoiduna alusta lähtien.

## How to: (Kuinka tehdä:)
Käytä `cargo`, Rustin paketinhallintaa ja käännöstyökalua, uuden projektin luomiseen. Tässä muutama rivi koodia:

```Rust
// Asenna Cargo, jos se ei ole jo asennettu
// Käytä komentoa terminaalissa (komentorivillä)

// Luo uusi Rust-projekti nimeltä "tervetuloa_rustiin"
cargo new tervetuloa_rustiin

// Siirry uuteen hakemistoon
cd tervetuloa_rustiin

// Käännä ja aja ohjelma
cargo run

// Ohjelman lähdekoodi on tiedostossa `src/main.rs`
// Ohjelma tulostaa oletuksena "Hello, world!"
```

Kun ajet `cargo run`, näet seuraavan:
```
   Compiling tervetuloa_rustiin v0.1.0 (file:///path/to/tervetuloa_rustiin)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/tervetuloa_rustiin`
Hello, world!
```

## Deep Dive (Sukellus syvälle):
Cargo on Rustin de facto työkalu, jonka avulla hallitaan riippuvuuksia ja käännösvaiheita. Se syntyi yhdessä Rustin kanssa vuonna 2010, helpottamaan Rust-ohjelmien rakentamista ja jakelua. Vaihtoehtoina ovat raaka `rustc` käyttö tai ulkopuoliset työkalut, mutta Cargo on suositeltavin valinta.

Cargo luo projektin pohjarakenteen, joka sisältää `Cargo.toml`-määritystiedoston ja `src`-hakemiston. `Cargo.toml` määrittelee paketin tiedot ja riippuvuudet, ja `src/main.rs` on oletuslähtötiedosto ohjelmiin.

Projektien luontiprosessi on tärkeä, sillä se standardisoi kehitysympäristön ja minimoi konfiguraation vaivaa. Rust-projektien kanssa yleensä ei tarvitse murehtia monimutkaisista rakennusskripteistä tai tiedostojen järjestelystä – Cargo hoitaa nuo puolestasi.

## See Also (Katso myös):
- Rustin virallinen sivusto ja aloitusopas: https://www.rust-lang.org/learn/get-started
- Cargo-ohjeet ja dokumentaatio: https://doc.rust-lang.org/cargo/
- "The Rust Programming Language" -kirja: https://doc.rust-lang.org/book/
