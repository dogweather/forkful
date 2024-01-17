---
title:                "Tulostetaan debug-lähtöä"
html_title:           "Rust: Tulostetaan debug-lähtöä"
simple_title:         "Tulostetaan debug-lähtöä"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Painokkaan debug-tulosteen tulostaminen on yksinkertaisesti tapa tarkastella ohjelman suoritustietoja tai virheviestejä. Se on erityisen hyödyllistä, kun yritetään selvittää, miksi ohjelma ei toimi odotetulla tavalla. 

Ohjelmoijat käyttävät painokkaita debug-tulosteita koska se auttaa heitä tunnistamaan ja korjaamaan ohjelman ongelmakohdat. Sillä voidaan selvittää, mikä koodi toimii odotetulla tavalla ja mikä ei. Se on olennainen työkalu ohjelmointikehityksessä.

## Miten?

```Rust
fn main() {
    let x = 5;
    println!("Muuttujan x arvo on {}", x);
}
```

Tässä esimerkissä painokas debug-tuloste tulostaa "Muuttujan x arvo on 5" ohjelman suorituksen aikana. Tämä auttaa meitä vahvistamaan, että muuttuja x on asetettu oikeaan arvoon.

## Syväsukellus

Debug-tulosteiden käyttö on ollut yleinen ohjelmointikäytäntö jo pitkään. Se on nopea ja helppo tapa selvittää ohjelman suorituksen tiedot ja virheet.

Toinen tapa tarkastella ohjelman suoritusta on käyttää debuggaus-työkaluja, kuten GDB (GNU Project Debugger) tai lldb (LLVM Debugger). Nämä työkalut antavat ohjelmoijille mahdollisuuden pysäyttää ohjelman suorituksen tiettyyn kohtaan ja tutkia muuttujien ja koodin tilaa.

Rustissa debug-tulosteiden toteutus perustuu "Debug" traitiin. Tämä antaa ohjelmoijille mahdollisuuden määrittää, miten tietty tyyppi tulostetaan debug-tilassa. Tämä mahdollistaa myös tietojen muotoilun tulostettaviksi tarkoituksiin.

## Katso myös

- [Rustin debuggin käyttö](https://doc.rust-lang.org/std/macro.print.html)
- [Rustin virheenkorjaus ja debuggaus -opas](https://docs.rs/rust/1.54.0/rustc_data_structures/vagueness/debug_info/index.html)