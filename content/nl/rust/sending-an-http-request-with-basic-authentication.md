---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:08:26.012978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het versturen van een HTTP-verzoek met basisauthenticatie betekent dat er een gebruikersnaam en wachtwoord in de aanvraagheader worden gestopt om te bewijzen dat je toegang moet krijgen. We doen dit wanneer diensten er zeker van willen zijn dat jij het bent, en niet zomaar een Jan Modaal, die probeert toegang te krijgen tot spullen.

## Hoe:

Voeg eerst de benodigde crate toe aan je `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

Nu, hier is de Rust-code om een GET-verzoek te verzenden met basisauthenticatie:

```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "Aladdin";
    let password = "open sesame";
    
    let auth = Basic {
        username: user.into(),
        password: Some(password.into()),
    };
    
    let response = client
        .get("http://example.com/geheimen")
        .header(Authorization(auth))
        .send()
        .await?;
    
    let content = response.text().await?;
    println!("Antwoord: {}", content);
    
    Ok(())
}
```

Als het correct is, zal het de geheimen afdrukken. Je begrijpt het wel.

## Diepgaande Duik

Voor `reqwest` zag je mensen worstelen met `curl` in Rust. Het is alsof je een handzaag verkiest boven een kettingzaag. Basisauth, hoewel eenvoudig, is geen Fort Knox. Het is gewoon Base64 van "gebruikersnaam:wachtwoord" – geen encryptie, dus HTTPS is een must.

Alternatieven? OAuth 2.0 danst rondjes om Basis, door tokens aan te bieden in plaats van tastbare referenties. Toch is het complex. Dan is er nog Bearer-authenticatie, die tokens vasthoudt als een geheime handdruk.

Onder de motorkap is `reqwest` een high-level HTTP-client die goed samenspeelt met Rust's async-functies. De structuur 'Basic' creëert de header, 'Authorization' plaatst hem, en voilà, je klopt aan de deur van de server met een geheime fluistering.

## Zie Ook

Voor meer lore en tovenarij:

- reqwest documentatie: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- Begrijpen van HTTP Basis Toegangsauthenticatie: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Rust async programmeren: [https://rust-lang.github.io/async-book/](https://rust-lang.github.io/async-book/)
- rust base64 crate documentatie: [https://docs.rs/base64](https://docs.rs/base64)
