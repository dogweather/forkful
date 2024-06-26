---
date: 2024-01-20 17:44:53.258137-07:00
description: "How to: (Kuinka tehd\xE4:) Rustissa web-sivun lataaminen onnistuu usealla\
  \ tavalla, mutta t\xE4ss\xE4 k\xE4yt\xE4mme `reqwest`-kirjastoa, joka on helppok\xE4\
  ytt\xF6inen ja\u2026"
lastmod: '2024-04-05T21:53:57.915035-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Rustissa web-sivun lataaminen onnistuu usealla tavalla,\
  \ mutta t\xE4ss\xE4 k\xE4yt\xE4mme `reqwest`-kirjastoa, joka on helppok\xE4ytt\xF6\
  inen ja tehokas."
title: Verkkosivun lataaminen
weight: 42
---

## How to: (Kuinka tehdä:)
Rustissa web-sivun lataaminen onnistuu usealla tavalla, mutta tässä käytämme `reqwest`-kirjastoa, joka on helppokäyttöinen ja tehokas.

```Rust
// Lisää ensin Cargo.toml-tiedostoosi riippuvuudeksi reqwest
reqwest = "0.11"

// Sitten kirjoita koodisi:
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let body = reqwest::get("https://www.rust-lang.org/")
        .await?
        .text()
        .await?;

    println!("Web-sivun sisältö:\n{}", body);
    Ok(())
}
```

Esimerkin tulosteena on ladatun sivun HTML-koodi konsolissa.

## Deep Dive (Syväsykellus):
Historiallisesti tiedon lataamiseen verkosta käytettiin Rustissa `hyper`-kirjastoa, joka on edelleen `reqwest`-kirjaston ytimessä. Vaihtoehtoja `reqwest`ille ovat `curl` ja `hyper`, mutta näiden käyttö on monimutkaisempaa. `reqwest` käyttää sisäisesti asynkronista koodia hyödyntääkseen Rustin `async`-odotustekniikoita maksimaalisen suorituskyvyn saavuttamiseksi. Kirjasto hoitaa myös monet HTTP-protokollan nitty-grittyt, kuten yhteydenhallinnan ja virheenkäsittelyn.

## See Also (Katso myös):
- [reqwest-kirjaston dokumentaatio](https://docs.rs/reqwest)
- [Rustin asynkronisen ohjelmoinnin opas](https://rust-lang.github.io/async-book/)
- [hyper-kirjaston GitHub-repo](https://github.com/hyperium/hyper)
- [curl-kirjaston GitHub-repo](https://github.com/alexcrichton/curl-rust)
