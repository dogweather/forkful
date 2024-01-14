---
title:                "Rust: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Rust er et programmeringsspråk som har vokst i popularitet de siste årene på grunn av sin sikkerhet, effektivitet og høye ytelse. Det er derfor et godt valg for å utvikle nettapplikasjoner som håndterer store mengder data. Når du sender HTTP-forespørsler med basic authentication, kan Rust hjelpe deg med å håndtere autentiseringsprosessen på en trygg og effektiv måte. I denne bloggposten vil jeg vise deg hvordan du kan implementere dette i Rust.

## Slik gjør du det

For å sende en HTTP-forespørsel i Rust med basic authentication, trenger du først å importere `reqwest` biblioteket ved å legge til følgende linje i `Cargo.toml`-filen din:

```Rust
reqwest = { version = "0.11.1", features = ["json"] }
```

Deretter må du opprette et `Client`-objekt fra `reqwest`-biblioteket og legge til autentiseringsinformasjonen i en `Authorization`-header:

```Rust
use reqwest::header::HeaderValue;

let client = reqwest::Client::new();

let mut headers = reqwest::header::HeaderMap::new();
headers.insert(reqwest::header::AUTHORIZATION, HeaderValue::from_static("Basic <base64-encoded-username-password>"));
```

Du kan enkelt generere en base64 koding av brukernavn og passord ved å bruke `base64` biblioteket, som følger:

```Rust
use base64;

let username = "example";
let password = "password";

let encoded = base64::encode(format!("{}:{}", username, password));
```

Nå er du klar til å sende den faktiske HTTP-forespørselen ved å opprette en `RequestBuilder` og legge til `Authorization`-headere og andre nødvendige parametere:

```Rust
let request = client.post("http://www.example.com")
    .headers(headers)
    .body("Body of the request");

let response = request.send().unwrap();

println!("Response status: {}", response.status());
```

## Dykk ned i det

Når du sender en HTTP-forespørsel med basic authentication, bruker du en form for autentisering som krever at brukernavn og passord blir sendt i klartekst gjennom `Authorization`-headere. Dette kan være en sikkerhetsrisiko hvis ikke tilkoblingen er kryptert. Det er derfor viktig å bruke HTTPS ved å bruke `https` i URL-en din i stedet for `http` når du sender HTTP-forespørsler med basic authentication.

I tillegg bør du vurdere å implementere en form for autentisering som gir bedre sikkerhet, for eksempel OAuth eller JWT. Dette vil ikke bare sikre autentiseringen din, men også beskytte brukerens sensitivt informasjon.

## Se også

Her er noen nyttige ressurser for å lære mer om Rust og hvordan du kan sende HTTP-forespørsler med basic authentication:

- [Offisiell Rust-dokumentasjon](https://doc.rust-lang.org)
- [Reqwest-dokumentasjon](https://docs.rs/reqwest/0.11.1/reqwest/)
- [How to send HTTP requests in Rust](https://blog.logrocket.com/sending-http-requests-in-rust/)
- [Basic authentication article](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)