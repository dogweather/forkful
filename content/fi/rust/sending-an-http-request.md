---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on tapa, jolla ohjelmat pystyvät kommunikoimaan web-palvelimien kanssa. Ohjelmoijat lähettävät näitä pyyntöjä tietojen saamiseksi tai lähettämiseksi palvelimille.

## Kuinka näin:

Tässä on esimerkki siitä, kuinka voit lähettää GET-pyynnön Rust-ohjelmassa käyttäen `reqwest`-kirjastoa:

```rust
use reqwest;
use std::io::Read;

let mut res = reqwest::get("https://httpbin.org/get").unwrap();
let mut body = String::new();
res.read_to_string(&mut body).unwrap();

println!("Vastaus:\n{}", body);
```

## Deep Dive

HTTP-pyynnöt ovat olleet olemassa webin alkuajoista lähtien ja ovat edelleen yksi tärkeimmistä tavoista ohjelmien ja palvelimien väliseen tiedonsiirtoon. Rustissa on useita muita kirjastoja, joilla voit lähettää HTTP-pyyntöjä, mukaan lukien hyper, isahc ja surf.

HTTP-pyyntöjen käsittelyyn liittyvät tekniset yksityiskohdat riippuvat siitä, käytätkö synkronista vai asynkronista lähestymistapaa. Yllä oleva esimerkki on synkroninen, joka kerää kaiken datan ennen ohjelman etenemistä. Asynkronisessa mallissa, kuten `reqwest::async` -kirjastossa, voidaan jatkaa muiden tehtävien suorittamista datan hakiessa.

## Katso myös 

Lisätietoja HTTP-pyynnöistä ja niiden lähettämisestä Rustilla löydät seuraavista lähteistä:
- [Reqwest-kirjaston dokumentaatio](https://docs.rs/reqwest)
- [Asynkronisen ohjelmoinnin opas Rustissa](https://rust-lang.github.io/async-book/)
- [HTTP-spesifikaatio](https://tools.ietf.org/html/rfc2616)