---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:42.176575-07:00
description: "Kuinka: Rustin standardikirjasto (`std`) sis\xE4lt\xE4\xE4 toiminnallisuuden\
  \ hakemiston olemassaolon tarkistamiseksi `std::path::Path` ja `std::fs` moduulien\u2026"
lastmod: '2024-03-13T22:44:56.371487-06:00'
model: gpt-4-0125-preview
summary: "Rustin standardikirjasto (`std`) sis\xE4lt\xE4\xE4 toiminnallisuuden hakemiston\
  \ olemassaolon tarkistamiseksi `std::path::Path` ja `std::fs` moduulien kautta."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
Rustin standardikirjasto (`std`) sisältää toiminnallisuuden hakemiston olemassaolon tarkistamiseksi `std::path::Path` ja `std::fs` moduulien kautta. Tässä on yksinkertainen esimerkki käyttäen Rustin standardimenetelmää:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/polku/hakemistoon");
    if path.exists() && path.is_dir() {
        println!("Hakemisto on olemassa.");
    } else {
        println!("Hakemistoa ei ole olemassa.");
    }
}
```

Esimerkkilähtö, olettaen että hakemisto on olemassa:
```
Hakemisto on olemassa.
```

Monimutkaisemmissa skenaarioissa tai laajennetuissa ominaisuuksissa (kuten asynkroninen tiedostojärjestelmän toiminnot) saatat harkita kolmannen osapuolen kirjaston, kuten `tokio`n, käyttämistä sen asynkronisen `fs`-moduulin kanssa, erityisesti jos työskentelet asynkronisessa ympäristössä. Näin voisit saavuttaa saman `tokio`n kanssa:

Lisää ensin `tokio` `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Käytä sitten `tokio::fs`:ää tarkistaaksesi hakemiston olemassaolon asynkronisesti:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/polku/hakemistoon";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Hakemisto on olemassa.");
            } else {
                println!("Polku on olemassa, mutta se ei ole hakemisto.");
            }
        },
        Err(_) => println!("Hakemistoa ei ole olemassa."),
    }
}
```

Esimerkkilähtö, olettaen että hakemistoa ei ole olemassa:
```
Hakemistoa ei ole olemassa.
```

Nämä esimerkit korostavat, kuinka Rust ja sen ekosysteemi tarjoavat sekä synkronisia että asynkronisia lähestymistapoja hakemiston olemassaolon tarkistuksiin, palvelemalla laajaa valikoimaa ohjelmistokehityksen tarpeita.
