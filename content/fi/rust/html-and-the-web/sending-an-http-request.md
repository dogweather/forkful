---
date: 2024-01-20 18:00:43.370282-07:00
description: "L\xE4het\xE4mme HTTP-pyynn\xF6n vaihtaaksemme tietoja palvelimien kanssa.\
  \ Sit\xE4 k\xE4ytet\xE4\xE4n datan hakemiseen, l\xE4hett\xE4miseen ja web-palveluiden\
  \ hy\xF6dynt\xE4miseen."
lastmod: '2024-03-13T22:44:56.353564-06:00'
model: gpt-4-1106-preview
summary: "L\xE4het\xE4mme HTTP-pyynn\xF6n vaihtaaksemme tietoja palvelimien kanssa."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: - Kuinka:
Rustin käyttöön HTTP-pyyntöjen lähettämiseen tarvitset ulkoisen kirjaston, kuten `reqwest`. Asenna ensin `reqwest` lisäämällä se `Cargo.toml`-tiedostoon:

```toml
[dependencies]
reqwest = "0.11"
```

Esimerkkikoodi GET-pyynnön lähettämiseksi:

```rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://httpbin.org/get").await?;
    println!("Status: {}", response.status());

    let body = response.text().await?;
    println!("Body:\n\n{}", body);

    Ok(())
}
```

Kun ajat koodin, saat vastaukseksi palvelimen tilakoodin ja vastauksen sisällön.

## Deep Dive - Syväsukellus:
HTTP-pyynnöt ovat HTTP-protokollan peruskivi. Alun perin kehitetty 1990-luvun alussa, ne mahdollistavat tiedonvaihdon asiakkaan ja palvelimen välillä. Rustissa voidaan käyttää `std::net`-moduulia alhaisen tason verkko-operaatioihin tai ulkoisia kirjastoja, kuten `reqwest` tai `hyper`, korkeamman tason abstraktioille.

`reqwest` on synkroninen ja asynkroninen HTTP-asiakaskirjasto, joka helpottaa monia HTTP-operaatioita. Synkronisessa moodissa koodi on yksinkertaisempi, mutta se voi jumiutua odottaessaan vastausta. Asynkronisessa moodissa Rustin `async`/`.await` piirteet antavat mahdollisuuden ei-tukkeutuviin operaatioihin, jolloin palvelin voi käsitellä muita pyyntöjä samanaikaisesti.

## See Also - Katso Myös:
- Virallinen `reqwest` kirjasto dokumentaatio: https://docs.rs/reqwest/
- Rust `async`/`.await` oppaat: https://rust-lang.github.io/async-book/
- HTTP-protokollan ymmärrys: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Rust `std::net` moduuli: https://doc.rust-lang.org/std/net/index.html

Tämä antaa sinulle hyvän pohjan aloittaa HTTP-pyyntöjen kanssa Rust-ohjelmoinnissa. Tutkiskele dokumentaatioita ja kokeile itse – käytännön kokemus on paras tapa oppia.
