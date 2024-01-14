---
title:                "Rust: Lukeminen komentorivin argumenteista"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat lukea komentorivin argumentteja Rust-ohjelmassa. Esimerkiksi, voit käyttää niitä ohjelman alkuperäisten asetusten määrittämiseen tai käyttäjän syötteen käsittelyyn.

## Miten

Rust tarjoaa `std::env`-moduulin, jonka avulla voit käsitellä komentorivin argumentteja. Käytämme `args()`-funktiota, joka palauttaa `Args`-iteraattorin. Tämän iterattorin avulla voit käydä läpi kaikki annetut argumentit.

```Rust
use std::env; // Tarvitaan 'env'-moduulin käyttämiseksi

fn main() {
    // Tallennetaan args()-funktion palauttama iterattori
    let args: Vec<String> = env::args().collect();

    // Käydään läpi kaikki annetut argumentit ja tulostetaan ne
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Ajettuna komennolla `./hello hello world`, ohjelma tulostaa seuraavan:

```bash
hello
hello
world
```

## Syvempi sukellus

`std::env`-moduuli tarjoaa myös muita käteviä toimintoja, kuten `current_exe()`, joka palauttaa polun nykyiseen suoritettavaan tiedostoon, tai `var_os()`, joka palauttaa ympäristömuuttujan arvon `OsString`-muodossa.

## Katso myös

- [Rustin opas syötteiden käsittelyyn](https://doc.rust-lang.org/std/env/index.html)
- [env-moduulin dokumentaatio](https://doc.rust-lang.org/std/env/index.html)
- [Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/fi/)