---
title:                "Lähetetään http-pyyntö"
html_title:           "Rust: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat lähettää HTTP-pyynnön? Koska sinun täytyy kommunikoida internetin kanssa! HTTP-pyynnöt ovat tapa lähettää ja vastaanottaa tietoa verkon yli, ja niitä käytetään esimerkiksi verkkosivustojen selaamiseen ja tietojen hakemiseen.

## Miten

Onneksi Rustilla on erittäin helppo lähettää HTTP-pyyntöjä. Sinun tarvitsee vain käyttää "reqwest" -kirjastoa ja sen avulla luoda "Client" -objekti. Sitten voit käyttää "get" -funktiota ja antaa sille URL-osoitteen, jonka haluat hakea. Lopuksi voit kutsua "send" -funktiota ja saada vastauksen takaisin "Response" -objektina, jossa voit käyttää erilaisia metodeja kuten "text" tai "json" saadaksesi haluamasi tiedot.

```Rust
use reqwest::Client;

let client = Client::new();
let response = client.get("https://www.example.com").send().unwrap();

println!("{}", response.text().unwrap());
```

Tämä koodi lähettää GET-pyynnön "www.example.com" -osoitteeseen ja tallentaa vastauksen "response" -muuttujaan. Sen jälkeen voimme käyttää "text" -metodia saadaksemme vastauksen tekstimuodossa ja tulostaa sen konsoliin.

## Syventävä tieto

Lisätietoja HTTP-pyyntöjen lähettämisestä Rustilla löytyy "reqwest" -kirjaston dokumentaatiosta ja Rustin standardikirjaston "std::io::Read" -dokumentaatiosta. Voit myös tutustua REST-arkkitehtuuriin ja HTTP-metodien käyttöön tarkemmin saadaksesi paremman käsityksen siitä, miten HTTP-pyyntöjä käytetään ja miksi.

## Katso myös

- "reqwest" -kirjaston dokumentaatio: https://docs.rs/reqwest/*/reqwest/
- Rustin "std::io::Read" -dokumentaatio: https://doc.rust-lang.org/std/io/trait.Read.html
- REST-arkkitehtuurin perusteet: https://restfulapi.net/
- HTTP-metodien selitykset ja käyttö: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods