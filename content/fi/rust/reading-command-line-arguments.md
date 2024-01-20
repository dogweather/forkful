---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Rust-ohjelmointi: Komentoriviparametrien Luku

## What & Why?

Komentoriviparametrien luku on data-annin keino, jossa käyttäjä syöttää tiedon suoraan ohjelman ajettaessa. Se antaa ohjelmalle joustavuutta ja tarkkuutta sovellusten suorittamiseen ja datan käsittelyyn.

## Miten:

Rustissa käytetään std::env:n funktiota args, joka palauttaa iteroinnin, joka sisältää ohjelman argumentit. Tässä on yksinkertainen esimerkki:

```Rust
fn main() {
    let args: Vec<String> = std::env::args().collect();

    println!("{:?}", args);
}
```

Suorita ohjelma komennolla `cargo run arg1 arg2`, ja saat tulokseksi: `["target/debug/program", "arg1", "arg2"]`.

## Syvällisempi tarkastelu

Historiallisesti, komentoriviparametrien luku on ollut alusta saakka osa ohjelmoinnin perustoimintoja. Näiden argumenttien avulla voidaan ohittaa oletusasetukset, määrittää tiedostopolkuja ja asettaa erilaisia suoritusparametreja.

Rustissa on useita erilaisia tapoja komentoriviparametrien lukemiseen. esim. käyttöön `getopts` tai `clap` kirjastoja, jotka antavat laajemmat mahdollisuudet kuten vaikkapa virheenkäsittelyyn ja lippujärjestelmän rakentamiseen.

Rustin `std::env::args` funktion tekeminen on yksinkertaista: alustaa Vec<String>:n, johon kerätään argumentit suoritettavasta ohjelmasta.

## Katso myös

- [Rustin virallinen dokumentaatio komentoriviparametreistä](https://doc.rust-lang.org/std/env/fn.args.html)
- [`clap`-kirjaston dokumentaatio](https://docs.rs/clap/2.33.3/clap/)