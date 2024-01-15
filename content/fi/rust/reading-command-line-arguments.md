---
title:                "Puuttujien komentoriviparametrien lukeminen"
html_title:           "Rust: Puuttujien komentoriviparametrien lukeminen"
simple_title:         "Puuttujien komentoriviparametrien lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi vaivautuisit lukemaan komentoriviparametreja Rustilla? Komentoriviparametrit ovat tapa välittää tietoa ohjelmalle ennen sen suorittamista. Näiden avulla voit esimerkiksi määrittää, mitä tiedostoja ohjelma käsittelee tai mikä toiminto suoritetaan.

## Kuinka

Käytössäsi saattaa olla ohjelma, joka vaatii komentoriviparametreja. Tällöin sinun täytyy lukea nämä parametrit ohjelman suorittamista varten. Alla on muutamia esimerkkejä Rustilla kirjoitettuna:

```Rust
use std::env;

// Hakee ensimmäisen parametrin
let argument = env::args().nth(1);

// Tulostaa toisen parametrin
let second_argument = env::args().nth(2);
println!("Toinen parametri: {}", second_argument);
```

Jos haluat käsitellä kaikki parametrit yhdessä, voit käyttää `args()`-metodia ja `collect()`-funktiota:

```Rust
use std::env;

// Hakee kaikki parametrit ja tallentaa ne vektroille
let arguments: Vec<String> = env::args().collect();

// Tulostaa kaikki parametrit
println!("Komentoriviparametrit: {:?}", arguments);
```

## Syväsukellus

Rustilla on käytössä `std::env`-kirjasto, joka sisältää monia hyödyllisiä toimintoja komentoriviparametrien käsittelyyn. Voit esimerkiksi käyttää `args_os()`-metodia, joka palauttaa `OsString`-tyyppisen vektorin parametreista. Tämä on hyödyllistä, jos haluat säilyttää parametrien alkuperäisen muodon.

Voit myös käyttää `current_dir()`-metodia saadaksesi polun nykyiseen työhakemistoon tai `var_os()`-metodia hakemaan ympäristömuuttujan arvon.

## Katso myös

- [Rustin virallinen dokumentaatio komentoriviparametreista](https://doc.rust-lang.org/std/env/#command-line-arguments)
- [Hyödyllisiä Rust-komentorivikirjastoja](https://www.rust-lang.org/learn/cli-crates)
- [Keskustelua Rust-komentoriviparametreista Rust-keskustelufoorumilla](https://users.rust-lang.org/t/command-line-arguments/4145)