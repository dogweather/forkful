---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Rust projektit. Mikä ja miksi?

Alussa on tyhjää. Uuden projektin aloittaminen tarkoittaa tyhjän tilan ottamista ja sen täyttämistä koodilla. Koodarit tekevät tämän ongelman ratkaisemiseksi tai idean toteuttamiseksi.

# Miten

Aloitetaan uusi Rust-projekti käyttämällä `cargo` työkalua. Syötä seuraavat komennot komentorivillä:

```Rust
$ cargo new amazing_project
$ cd amazing_project
```

Tämä luo uuden kansio `amazing_project` nimisen projektin. Siirry tämän jälkeen projektiin `cd` komennon avulla.

# Syvä sukellus

`cargo new` komento on ollut osa Rustia versiosta 0.12.0 lähtien, jolloin Cargo esiteltiin Rust projektien hallintajärjestelmänä.

Vaihtoehtoja `cargo new` komennolle ovat muut projektisovellukset, kuten `cargo-generate` tai `kickstart`, mutta `cargo new` on Rust yhteisön vakio.

Sisällä, `cargo new` luo uuden kansio struktuurin `src` kansio ja `Cargo.toml` tiedosto. `src` kansio sisältää aloitus tiedoston `main.rs` tai `lib.rs`, jos kyseessä on kirjasto projekti. `Cargo.toml` tiedosto määrittelee projektin pakettiasetukset sekä riippuvuudet.

# Katso myös

Tätä informaatiota ja lisäohjeita voi löytää seuraavista lähteistä:

- [Rust Book](https://doc.rust-lang.org/book/title-page.html) on erittäin hyödyllinen resurssi, minkä tahansa Rust-aiheen tutkimiseen ja ymmärtämiseen.
- [Cargo Documentation](https://doc.rust-lang.org/cargo/guide/) antaa syvällisen kuvauksen työkalun ominaisuuksista ja käytöstä.