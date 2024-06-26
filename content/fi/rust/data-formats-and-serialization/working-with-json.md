---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:07.540105-07:00
description: "Kuinka: JSONin k\xE4sittelyyn Rustissa k\xE4ytet\xE4\xE4n laajalti `serde`-pakettia\
  \ yhdess\xE4 `serde_json`in kanssa sarjoittamiseen ja j\xE4sent\xE4miseen. Ensin,\
  \ varmista\u2026"
lastmod: '2024-03-13T22:44:56.378661-06:00'
model: gpt-4-0125-preview
summary: "JSONin k\xE4sittelyyn Rustissa k\xE4ytet\xE4\xE4n laajalti `serde`-pakettia\
  \ yhdess\xE4 `serde_json`in kanssa sarjoittamiseen ja j\xE4sent\xE4miseen."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Kuinka:
JSONin käsittelyyn Rustissa käytetään laajalti `serde`-pakettia yhdessä `serde_json`in kanssa sarjoittamiseen ja jäsentämiseen. Ensin, varmista että sisällytät nämä `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Esimerkki 1: Jäsenetään JSON Rust-rakenteeksi
Määrittele Rust-rakenne ja käytä johdannaisia makroja `Deserialize`lle ja `Serialize`lle:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("Käyttäjän ID: {}", user.id);
    println!("Käyttäjän nimi: {}", user.name);
    println!("Käyttäjän sähköposti: {}", user.email);
}
```

**Tuloste:**

```
Käyttäjän ID: 1
Käyttäjän nimi: Jane Doe
Käyttäjän sähköposti: jane.doe@example.com
```

### Esimerkki 2: Sarjoitetaan Rust-rakenne JSONiksi
Käyttäen samaa `User`-rakennetta:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Tuloste:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Nämä esimerkit havainnollistavat perusprosessin JSONin jäsentämisestä Rust-rakenteiksi ja Rust-rakenteiden sarjoittamisesta takaisin JSON-merkkijonoiksi. Serde tarjoaa runsaan valikoiman työkaluja JSONin käsittelyyn, mukaan lukien valinnaisten kenttien, monimutkaisten pesintöjen ja JSONin suoraan tukemattomien tyyppien käsittelyn.
