---
date: 2024-01-20 18:02:34.966925-07:00
description: "Mik\xE4 ja miksi? HTTP-pyynt\xF6 perusautentikaatiolla tarkoittaa palvelimelle\
  \ l\xE4hetett\xE4v\xE4\xE4 pyynt\xF6\xE4, jossa k\xE4ytt\xE4j\xE4nimi ja salasana\
  \ ovat mukana. Ohjelmoijat\u2026"
lastmod: 2024-02-19 22:05:15.251725
model: gpt-4-1106-preview
summary: "Mik\xE4 ja miksi? HTTP-pyynt\xF6 perusautentikaatiolla tarkoittaa palvelimelle\
  \ l\xE4hetett\xE4v\xE4\xE4 pyynt\xF6\xE4, jossa k\xE4ytt\xE4j\xE4nimi ja salasana\
  \ ovat mukana. Ohjelmoijat\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? HTTP-pyyntö perusautentikaatiolla tarkoittaa palvelimelle lähetettävää pyyntöä, jossa käyttäjänimi ja salasana ovat mukana. Ohjelmoijat käyttävät tätä metodia päästäkseen käsiksi suojattuihin resursseihin ja API-palveluihin.

## How to:
Miten toteutetaan:

```Rust
use reqwest::{blocking::Client, header};

fn main() -> Result<(), reqwest::Error> {
    let client = Client::new();
    let creds = "username:password";
    let base64_creds = base64::encode(creds);

    let response = client
        .get("http://example.com/protected")
        .header(header::AUTHORIZATION, format!("Basic {}", base64_creds))
        .send()?;

    println!("Status: {}", response.status());
    println!("Body:\n{}", response.text()?);
    Ok(())
}
```
Esimerkki tuloste:
```
Status: 200 OK
Body:
{ "some": "json", "with": "data" }
```

## Deep Dive
Syväsukellus: Alun perin lisätty HTTP/1.0:een, perusautentikaatio on nopea ja yksinkertainen tapa suojata sisältö. Vaikka perusautentikaatio on helppo toteuttaa, se on turvallinen vain HTTPS:n kanssa. Alternatiiveina ovat kehittyneemmät autentikaatiomenetelmät kuten OAuth, joka mahdollistaa pääsyn ilman suoria kirjautumistietoja. Tietoturvasyistä käyttäjätunnus ja salasana koodataan Base64-muotoon, mutta huomaa, että ilman HTTPS:ää tämä ei ole turvassa välimiesten hyökkäyksiltä.

## See Also
Katso myös:
- [reqwest crate documentation](https://docs.rs/reqwest/)
- [Rust standard library documentation on Base64](https://doc.rust-lang.org/stable/std/primitive.slice.html#method.from_utf8)
- [Mozilla Developer Network (MDN) - Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
