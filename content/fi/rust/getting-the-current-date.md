---
title:                "Tämänhetkisen päivämäärän hankkiminen"
html_title:           "Rust: Tämänhetkisen päivämäärän hankkiminen"
simple_title:         "Tämänhetkisen päivämäärän hankkiminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Haluatko ohjelmassasi käyttää ajanmukaista päivämäärää? Ei hätää, Rustin avulla se onnistuu helposti! Ohjelmoijat käyttävät tätä toimintoa monissa sovelluksissa, kuten aikaleimoissa ja ajastimissa.

## Miten:
```Rust
use std::time::SystemTime;

fn main() {
    let now = SystemTime::now();

    match now.duration_since(SystemTime::UNIX_EPOCH) {
        Ok(n) => println!("Current timestamp: {}", n.as_secs()),
        Err(_) => panic!("SystemTime before UNIX EPOCH!"),
    }
}
```
Tämän yksinkertaisen koodin avulla voit hakea nykyisen ajan ja muuttaa sen aikaleimaksi. Saat tulosteena myös timestampin sekunteina UNIX EPOCH -ajasta.

## Syväsukellus:
Getting the current date is an essential function in most programming languages. In Rust, it is achieved through the SystemTime type and its associated functions. An alternative way to get the current date in Rust is to use the chrono crate, which provides a higher level interface for handling dates and times. The implementation of getting the current date in Rust is platform-dependent, as it relies on the underlying operating system's functions.

## Katso myös:
- [Rustin dokumentaatio](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Chrono crate](https://crates.io/crates/chrono)
- [UNIX EPOCH](https://en.wikipedia.org/wiki/Unix_time)