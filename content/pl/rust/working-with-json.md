---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
W Polsce JSON to standard wymiany danych między aplikacjami. Programiści używają JSON, bo jest lekki, elastyczny i szeroko wspierany.

## How to:
Zainstaluj crate `serde_json` dla parsowania i serializacji:

```toml
[dependencies]
serde = "1.0"
serde_json = "1.0"
serde_derive = "1.0"
```

Struktura i konwersja do/from JSON:

```Rust
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
struct Użytkownik {
    id: u64,
    nazwa: String,
    wiek: u8,
    aktywny: bool,
}

fn main() {
    let json_data = r#"
    {
        "id": 1,
        "nazwa": "Jan Kowalski",
        "wiek": 30,
        "aktywny": true
    }"#;

    // Deserializacja
    let u: Użytkownik = serde_json::from_str(json_data).unwrap();

    // Serializacja
    let json = serde_json::to_string(&u).unwrap();
    println!("Serializacja JSON: {}", json);
}
```

Sample output:

```
Serializacja JSON: {"id":1,"nazwa":"Jan Kowalski","wiek":30,"aktywny":true}
```

## Deep Dive
JSON, czyli JavaScript Object Notation, powstał w latach 2000. Alternatywą jest XML, ale JSON wygrał popularnością. Implementacja w Rust z `serde_json` używa prostych adnotacji do zarządzania serializacją/deserializacją i jest efektywna.

## See Also
- [Serde Official Documentation](https://serde.rs)
- [Rust Programming Language Official Website](https://www.rust-lang.org/learn)