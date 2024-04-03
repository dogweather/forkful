---
date: 2024-01-20 17:51:32.803090-07:00
description: "Kuinka tehd\xE4: ."
lastmod: '2024-03-13T22:44:56.343087-06:00'
model: gpt-4-1106-preview
summary: .
title: Merkkijonon interpolointi
weight: 8
---

## Kuinka tehdä:
```Rust
fn main() {
    let muuttuja = "maailma";
    let viesti = format!("Hei, {}!", muuttuja);
    println!("{}", viesti); // Tulostaa: Hei, maailma!
}
```

```Rust
fn main() {
    let vuosi = 2023;
    println!("On vuosi {} ja Rust on edelleen mahtava!", vuosi); // Tulostaa: On vuosi 2023 ja Rust on edelleen mahtava!
}
```

## Syväsukellus:
Historiallisesti string-interpolointi on ollut ominaisuus monissa ohjelmointikielissä ennen Rustia, kuten Perl, Python ja Ruby. Rustissa `format!` makro ottaa käyttöön interpoloinnin, mikä on turvallinen tapa yhdistellä merkkijonoja, sillä se estää muistivirheet ja pistesijoitusvirheet. Vaihtoehtona `format!`, `println!` makroa voi käyttää suoraan merkkijonojen tulostukseen interpoloinnin kanssa. Toisin kuin jotkut muut kielet, Rustissa ei käytetä erityistä merkintää, kuten `$` string-interpoloinnissa, mikä voi vähentää sekaannusta.

Suorituskyvyn kannalta, koska Rust on alhaisen tason kieli, interpolointi `format!` makrolla voi olla hitaampi kuin konkatenaatio käytettäessä pieniä kiinteitä merkkijonoja, mutta ero ei yleensä ole merkittävä. Käyttö `format!` makroa tekee koodin selvemmin ymmärrettäväksi ja ylläpidettäväksi, joten se on hyvä käytäntö.

## Katso myös:
- Rustin virallinen dokumentaatio string-formaateista: [https://doc.rust-lang.org/std/fmt/](https://doc.rust-lang.org/std/fmt/)
- Rust book, jossa käsitellään stringejä ja niiden manipulointia: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust by Example, formatointi-osio: [https://doc.rust-lang.org/rust-by-example/hello/print/fmt.html](https://doc.rust-lang.org/rust-by-example/hello/print/fmt.html)
