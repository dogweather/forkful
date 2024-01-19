---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran med grundläggande autentisering innebär att lägga till offentlig och hemlig identifieringsinformation i din webbansökan för att säkerställa att bara auktoriserade användare får åtkomst. Programmerare gör detta för att skydda känslig data och funktioner från obehörig åtkomst.

## Hur Man Gör:

Använd `reqwest`-klienten för att skicka en HTTP-begäran med grundläggande autentisering i Rust. Först, installera paketet `reqwest` med funktionen `blocking` aktiverad i din `Cargo.toml` fil.

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking"] }
base64 = "0.13"
```

Här är ett exempel på hur du skickar en GET-begäran till en skyddad resurs.

```Rust
use reqwest::blocking::Client;
use base64::encode;

fn send_request() -> Result<(), reqwest::Error> {
    let client = Client::new();
    let user = "user";
    let pass = "password";
    let auth = format!("{}:{}", user, pass);
    let basic_auth = format!("Basic {}", encode(&auth));

    let resp = client
        .get("http://example.com")
        .header(reqwest::header::AUTHORIZATION, basic_auth)
        .send()?;

    println!("{:?}", resp.status());

    Ok(())
}
```

När du kör detta skript kommer HTTP-statuskoden att skrivas ut på konsolen.

## Djupdykning:

Grundläggande autentisering är en äldre metod för autentisering som ursprungligen definierades i HTTP/1.0-specifikationen. Det är känd för sin enkelhet, men har många säkerhetsbrister såsom klar-text lösenord. 

Alternativ för grundläggande autentisering inkluderar mermodern OAuth2.0 eller JWT(Jason Web Tokens) autentisering, som erbjuder robust säkerhet genom att hålla lösenorden krypterade.

Vid implementering, se till att lösenord och andra känsliga uppgifter skyddas med korrekt kryptering och lagring, eftersom de kommer att avslöjas om HTTP-trafiken avlyssnas. 

Om vi djupdyker i koden lite mer, använder vi `reqwest::header::AUTHORIZATION` för att ange HTTP-huvudet för autentisering och `base64::encode` för att koda våra autentiseringsuppgifter.

## Se Också:

Rust reqwest dokumentation: https://docs.rs/reqwest/0.11.6/reqwest/
Om grundläggande autentisering: https://sv.wikipedia.org/wiki/Basic_access_authentication
Alternativ för grundläggande autentisering: https://auth0.com/blog/cookies-vs-tokens-definitive-guide