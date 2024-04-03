---
date: 2024-01-20 18:04:56.012937-07:00
description: "How to: (Kuinka tehd\xE4:) K\xE4yt\xE4 `cargo`, Rustin paketinhallintaa\
  \ ja k\xE4\xE4nn\xF6sty\xF6kalua, uuden projektin luomiseen. T\xE4ss\xE4 muutama\
  \ rivi koodia."
lastmod: '2024-03-13T22:44:56.357427-06:00'
model: gpt-4-1106-preview
summary: "K\xE4yt\xE4 `cargo`, Rustin paketinhallintaa ja k\xE4\xE4nn\xF6sty\xF6kalua,\
  \ uuden projektin luomiseen."
title: Uuden projektin aloittaminen
weight: 1
---

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
