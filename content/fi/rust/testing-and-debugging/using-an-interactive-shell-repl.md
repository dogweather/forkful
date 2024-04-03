---
date: 2024-01-26 04:18:21.115907-07:00
description: "Miten: T\xE4ll\xE4 hetkell\xE4 Rust ei toimita virallista REPLi\xE4\
  . Voit k\xE4ytt\xE4\xE4 kolmannen osapuolen ty\xF6kaluja, kuten `evcxr_repl`. Asenna\
  \ se Cargo:n avulla."
lastmod: '2024-03-13T22:44:56.358305-06:00'
model: gpt-4-0125-preview
summary: "T\xE4ll\xE4 hetkell\xE4 Rust ei toimita virallista REPLi\xE4."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

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
