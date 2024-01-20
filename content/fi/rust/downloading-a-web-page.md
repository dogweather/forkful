---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Lataa Web-sivu Rustilla: Pikaopas

## Mikä & Miksi?
Web-sivun lataaminen tarkoittaa sivun HTML-tiedoston hankkimista verkolta omalle laitteelle. Ohjelmoijat tekevät tämän syystä, esimerkiksi verkkosivun sisällön analysoinniin tai testaamiseen.

## Näin sen teet:

`reqwest` -kirjasto tekee web-sivun lataamisesta helppoa Rust-ohjelmointikiellolla. 

```Rust
use reqwest;
use std::fs;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://www.example.com").await?;
    fs::write("example.html", response.text().await?).expect("Unable to write file");
    Ok(())
}
```

Tämä koodi lataa HTML:n osoitteesta `https://www.example.com`, ja tallentaa sen tiedostoon `example.html`.

## Sukellus syvyyksiin
Vaikka web-sivun lataaminen onnistuu alla olevalla esimerkillä, sen takana on paljon enemmän. Historiallisesti lataaminen tehtiin kutsumalla suoraan alhaisen tason verkkosovittimia ja kirjoittamalla vastaanotettu data kovalevylle. Alternatiiveja, kuten `hyper`-kirjastoa, pidetään myös ajantasaisena, mutta `reqwest` tarjoaa yksinkertaisen ja modernin käyttöliittymän samaan tarkoitukseen.

Käytettäessä `reqwest` kirjastoa, sinun tulisi tietää, että `reqwest::get` -kutsu palauttaa `Response` -olion, joka on yhteyden yhteydessä serveriin. `Response.text().await?` palauttaa vastauksen merkkijonona. Viimeisenä, Rustin `fs::write` kirjoittaa tiedoston levylle.

## Katso myös
1. Reqwest library: [linkki](https://docs.rs/reqwest)
2. Async programming in Rust: [linkki](https://rust-lang.github.io/async-book)
3. Hyper library: [linkki](https://hyper.rs/)
4. Rust Programming Language Book FileSystem section: [linkki](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)

Nämä lähteet tarjoavat laajemman ymmärryksen Rustilla ohjelmoinnista ja erityisesti web-sivun lataamisesta.