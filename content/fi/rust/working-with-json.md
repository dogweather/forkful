---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on kevyt dataformaatti tiedonvaihtoon. Rust-koodaajat käyttävät sitä datan helppoon serialisointiin ja deserialisointiin, mikä tekee palvelinten ja selainten välisestä kommunikaatiosta vaivatonta.

## How to:
```Rust
use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct User {
    username: String,
    email: String,
}

fn main() {
    let json_str = r#"{"username":"taikuri","email":"[email protected]"}"#;

    // Deserialisoi JSON-string Rust-structiin
    let user: User = serde_json::from_str(json_str).unwrap();
    println!("Deserialisoitu: {:?}", user);

    // Serialisoi Rust-struct takaisin JSON-stringiin
    let serialized = serde_json::to_string(&user).unwrap();
    println!("Serialisoitu: {}", serialized);
}
```
Sample output:
```
Deserialisoitu: User { username: "taikuri", email: "[email protected]" }
Serialisoitu: {"username":"taikuri","email":"[email protected]"}
```

## Deep Dive
JSON on syntynyt 2000-luvun alussa ja on noussut XML:n rinnalle ja ohi suosiossa datan esittämiseksi. Rust tarjoaa serde-kirjastoa, joka on tehokas työkalu JSON:n käsittelyyn. Erilaisia formaatteja, kuten Toml ja YAML, käytetään myös, mutta JSON pysyy suosituimpana verkkopalveluissa datan ansiosta sen yksinkertaisuuden ja laajan tuen takia. Rustissa serde-koodin generointi hoituu derive-makron avulla automaattisesti, mikä säästää aikaa.

## See Also
- Serde-ohjekirja: https://serde.rs/
- Rustin viralliset dokumentit serde_json: https://docs.serde.rs/serde_json/
- JSONin viralliset sivut: https://www.json.org/json-en.html
