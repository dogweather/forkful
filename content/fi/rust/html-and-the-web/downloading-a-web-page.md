---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/rust/downloading-a-web-page.md
date:                  2024-01-20T17:44:53.258137-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Web-sivun lataaminen tarkoittaa sivun sisällön noutamista internetistä. Ohjelmoijat tekevät tätä esimerkiksi datan keräämiseen, sisällön analysointiin tai palveluiden automatisointiin.

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
