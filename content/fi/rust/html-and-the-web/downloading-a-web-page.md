---
date: 2024-01-20 17:44:53.258137-07:00
description: "Web-sivun lataaminen tarkoittaa sivun sis\xE4ll\xF6n noutamista internetist\xE4\
  . Ohjelmoijat tekev\xE4t t\xE4t\xE4 esimerkiksi datan ker\xE4\xE4miseen, sis\xE4\
  ll\xF6n analysointiin tai\u2026"
lastmod: '2024-03-13T22:44:56.355591-06:00'
model: gpt-4-1106-preview
summary: "Web-sivun lataaminen tarkoittaa sivun sis\xE4ll\xF6n noutamista internetist\xE4\
  ."
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
