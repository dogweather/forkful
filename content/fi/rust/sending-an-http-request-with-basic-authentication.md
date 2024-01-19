---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-pyyntöjen lähettäminen perusautentikoinnilla Rust-ohjelmointikielessä
---

## Mikä & Miksi?

HTTP-pyyntöjen lähettäminen perusautentikoinnin avulla tarkoittaa palvelimelle lähetettäviä pyyntöjä, jotka sisältävät autentikointitietoja. Ohjelmoijat tekevät tämän yleensä, kun heidän on päästävä käsiksi suojattuihin resursseihin.

## Kuinka näin:

Käytämme `reqwest`-kirjastoa, jolla voimme lähettää HTTP-pyyntöjä Rust-ohjelmassa. 

```Rust
use reqwest::{Client, Error};

async fn send_request() -> Result<(), Error> {
    let client = Client::new();
    let resp = client 
        .get("https://website.com/authenticated-endpoint")
        .basic_auth("your-username", Some("your-password"))
        .send().await?;
    
    println!("{:?}", resp.text());
    Ok(())
}
```
Koodi luo uuden asiakkaan, lähettää GET-pyynnön autentikointitietojen avulla ja tulostaa vastauksen.

## Syvällisempi sukellus

Perusautentikointi on yksi yksinkertaisimmista tavoista suorittaa autentikointi HTTP-pyynnöissä, ja se on ollut käytössä jo varhaisista verkkopäivistä lähtien. Yksinkertaisuudestaan huolimatta se on edelleen suosittu, koska sitä on helppo toteuttaa ja se on riittävän turvallinen useimmissa tapauksissa.

Vaihtoehtoisia menetelmiä ovat kehittyneemmät autentikointijärjestelmät, kuten OAuth tai JWT, joita voi käyttää monimutkaisemmissa ja turvallisemmissa järjestelmissä.

Rust-ohjelman HTTP-pyynnön lähettämisen toteutus perusautentikoinnin kanssa tarkoittaa käytännössä `reqwest`-kirjaston `basic_auth`-metodin kutsumista, joka lisää asianmukaisen tiedon HTTP-pyynnön otsakkeisiin.

## Katso myös

- Rust-ohjelmointikieli: https://www.rust-lang.org/
- reqwest-kirjasto: https://docs.rs/reqwest
- HTTP-perusautentikointi: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication