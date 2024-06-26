---
date: 2024-01-20 18:02:34.966925-07:00
description: "How to: Syv\xE4sukellus: Alun perin lis\xE4tty HTTP/1.0:een, perusautentikaatio\
  \ on nopea ja yksinkertainen tapa suojata sis\xE4lt\xF6. Vaikka perusautentikaatio\
  \ on\u2026"
lastmod: '2024-04-05T22:51:10.506560-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4sukellus: Alun perin lis\xE4tty HTTP/1.0:een, perusautentikaatio\
  \ on nopea ja yksinkertainen tapa suojata sis\xE4lt\xF6. Vaikka perusautentikaatio\
  \ on helppo toteuttaa, se on turvallinen vain HTTPS:n kanssa. Alternatiiveina ovat\
  \ kehittyneemm\xE4t autentikaatiomenetelm\xE4t kuten OAuth, joka mahdollistaa p\xE4\
  \xE4syn ilman suoria kirjautumistietoja. Tietoturvasyist\xE4 k\xE4ytt\xE4j\xE4tunnus\
  \ ja salasana koodataan Base64-muotoon, mutta huomaa, ett\xE4 ilman HTTPS:\xE4\xE4\
  \ t\xE4m\xE4 ei ole turvassa v\xE4limiesten hy\xF6kk\xE4yksilt\xE4."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

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
