---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
aliases:
- /fi/rust/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:21.115907-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Rustin interaktiivinen kuori eli REPL (Lue-Arvo-Tulosta-Silmukka) mahdollistaa Rust-koodin suorittamisen lennossa, tarjoten välittömiä tuloksia, mikä on täydellistä kokeiluun tai oppimiseen. Ohjelmoijat käyttävät sitä koodinpätkien testaamiseen, vianetsintään tai vain kielen ominaisuuksien tutkimiseen ilman koko projektin kääntämisen vaivaa.

## Miten:
Tällä hetkellä Rust ei toimita virallista REPLiä. Voit käyttää kolmannen osapuolen työkaluja, kuten `evcxr_repl`. Asenna se Cargo:n avulla:

```sh
cargo install evcxr_repl
```

Käynnistä sitten REPL:

```sh
evcxr
```

Sisällä, testaa Rust-koodia:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Tuloksen pitäisi olla:

```
5 + 3 = 8
```

## Syväsukellus
Rustin eetos keskittyy turvallisuuteen ja suorituskykyyn, jotka yleensä yhdistetään ajoa edeltävään kääntämiseen ja vähemmän tulkittuihin, REPL-ystävällisiin kieliin. Historiallisesti kielet kuten Python tai Ruby priorisoivat REPLin olemassaolon välittömän palautteen saamiseksi, mutta niitä ei suunniteltu järjestelmätason tehtäviin ajatellen.

Rustin virallisen REPLin puuttuessa on ilmennyt muutamia vaihtoehtoja, kuten `evcxr_repl`. Nämä projektit eivät vain sovita Rustia REPLiin; ne kutovat ovelasti yhteen kielen käännä-ja-suorita-syklin interaktiiviseen istuntoon. REPL kääntää koodin taustalla ja suorittaa binäärin, ottaen tulosteen talteen. Näin se säilyttää Rustin suorituskykyedut tarjoten silti interaktiivisen kokemuksen.

Rust-yhteisössä käydään jatkuvaa keskustelua virallisen REPL-tuen puolesta, ja jokaisen kielen iteroinnin myötä näemme lisää työkalujen kehittymistä, mikä saattaa lopulta johtaa natiiviin ratkaisuun.

## Katso myös
Lisätietoja ja muita työkaluja:
- Evcxr REPL GitHub-repositorio: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, verkossa tapa testata Rust-koodia: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Rust-kielen keskustelu REPL-ominaisuudesta: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
