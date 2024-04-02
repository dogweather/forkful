---
date: 2024-01-20 17:53:31.027454-07:00
description: "Debug-tulostus auttaa n\xE4kem\xE4\xE4n mit\xE4 koodissasi tapahtuu.\
  \ K\xE4yt\xE4mme sit\xE4 virheiden j\xE4ljitt\xE4miseen ja koodin toiminnan varmistamiseen."
lastmod: '2024-03-13T22:44:56.359221-06:00'
model: gpt-4-1106-preview
summary: "Debug-tulostus auttaa n\xE4kem\xE4\xE4n mit\xE4 koodissasi tapahtuu. K\xE4\
  yt\xE4mme sit\xE4 virheiden j\xE4ljitt\xE4miseen ja koodin toiminnan varmistamiseen."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## Mikä ja Miksi?
Debug-tulostus auttaa näkemään mitä koodissasi tapahtuu. Käytämme sitä virheiden jäljittämiseen ja koodin toiminnan varmistamiseen.

## Miten:
Rustissa debug-tulostus onnistuu `println!` makrolla:

```Rust
fn main() {
    let muuttuja = 42;
    println!("Debug-tulostus: {:?}", muuttuja);
}
```

Tulostuu:

```
Debug-tulostus: 42
```

Struktuurien kanssa käytä `derive(Debug)`:

```Rust
#[derive(Debug)]
struct OmaTietue {
    nimi: String,
    id: i32,
}

fn main() {
    let henkilo = OmaTietue {
        nimi: String::from("Matti"),
        id: 1234,
    };
    println!("Debug-tulostus: {:?}", henkilo);
}
```

Tulostuu:

```
Debug-tulostus: OmaTietue { nimi: "Matti", id: 1234 }
```

## Syväsukellus:
Debug-tulostus on ollut kauan ohjelmistokehityksen työkalu. Se on suoraan, helposti luettava tapa nähdä arvoja suorituksen aikana. Rustin `println!` makro tukee debug-tulostusta käyttäen `Debug` traitia. Sen avulla monimutkaisemmatkin tyypit voi tulostaa siististi, kunhan ne toteuttavat tai johdetaan `Debug` traitin. Vaihtoehtoina voidaan käyttää logitus-kirjastoja, kuten `log` tai `env_logger`.

## Katso Myös:
- Rustin `std::fmt` moduuli: https://doc.rust-lang.org/std/fmt/
- Rust Book aiheesta "Debug" tulostaminen: https://doc.rust-lang.org/book/ch05-02-example-structs.html#adding-useful-functionality-with-derived-traits
- Rust `log` kirjasto: https://crates.io/crates/log
- `env_logger` kirjasto: https://crates.io/crates/env_logger
