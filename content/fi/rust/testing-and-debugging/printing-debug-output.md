---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases:
- /fi/rust/printing-debug-output/
date:                  2024-01-20T17:53:31.027454-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

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
