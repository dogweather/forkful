---
title:    "Rust: Komantoriviparametrien lukeminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan Rust-ohjelmoinnin blogipostausta Suomenkielisille lukijoille! Tässä artikkelissa käsittelemme komentoriviparametrien lukemista ja kuinka se voidaan tehdä Rustilla.

## Miten

Komentoriviparametrien lukeminen on tärkeä osa monia ohjelmia, joilla on käyttäjän kanssa vuorovaikutusta. Onneksi Rustilla tämä on helppoa, sillä kielessä on valmiiksi sisäänrakennettu crate (kirjasto) nimeltä `std::env`, joka tarjoaa toimintoja komentoriviparametrien lukemiseen.

Alla on esimerkki koodista, joka lukee ja tulostaa komentoriviparametrit:

```Rust
use std::env;

fn main() {
    // Haetaan komentoriviparametrit ja tallennetaan ne muuttujaan `args`
    let args: Vec<String> = env::args().collect();

    // Tulostetaan komentoriviparametrit yksi kerrallaan
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Kun suoritamme tämän koodin komentorivillä, esimerkiksi komennolla `rustc args.rs && ./args hello world`, saamme seuraavan tulosteen:

```
./args
hello
world
```

Mikäli haluamme ottaa huomioon myös komentorivin ensimmäisen argumentin, joka on yleensä itse suoritettava tiedosto, voimme käyttää `skip()`-funktiota. Alla oleva koodi suorittaa saman tehtävän kuin edellinen, mutta jättää ensimmäisen argumentin huomioimatta:

```Rust
use std::env;

fn main() {
    // Haetaan komentoriviparametrit ja tallennetaan ne muuttujaan `args`
    let args: Vec<String> = env::args().skip(1).collect();

    // Tulostetaan komentoriviparametrit yksi kerrallaan
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

## Syväsukellus

Komentoriviparametrien lukeminen Rustilla tapahtuu siis `std::env` -craten avulla. Tämä kirjasto tarjoaa myös muita hyödyllisiä toimintoja, kuten `current_dir()`, joka palauttaa ohjelman nykyisen työhakemiston polun. Lisätietoa tästä kirjastosta löytyy Rustin virallisesta dokumentaatiosta.

## Katso myös

- [Rustin virallinen dokumentaatio - std::env](https://doc.rust-lang.org/std/env/index.html)
- [Rust by Example - Command Line Arguments](https://doc.rust-lang.org/stable/rust-by-example/std_misc/arg.html)
- [Rust in Peace - Parsing Command Line Arguments in Rust](https://www.rustinpeace.org/parsing_command_line_arguments_in_rust.html)