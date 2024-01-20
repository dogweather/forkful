---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering er en prosess hvor en bruker verifiserer sin identitet til serveren ved hjelp av brukernavn og passord. Programmerere gjør dette for å sørge for sikker dataoverføring og hindre uautorisert tilgang.

## Slik gjør du:

Å bruke `reqwest`-biblioteket er en enkel måte å gjøre dette på i Rust. Du kan legge til dette i dine `Cargo.toml`-avhengigheter:
```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
base64 = "0.10.1"
```
Og her er et eksempel på hvordan du kan sende en HTTP-forespørsel med basic autentisering.

```Rust
use reqwest::blocking::Client;
use base64::encode;

fn main() -> Result<(), Box<dyn std::error::Error>> {
     let client = Client::new();

     // Brukernavn og passord
     let user = "brukernavn";
     let password = "passord";
     let auth = encode(&format!("{}:{}", user, password));

     // Send forespørsel
     let res = client.get("https://eksempel.no")
                     .header("Authorization", format!("Basic {}", auth))
                     .send()?;

     println!("{}", res.status());
     Ok(())
}
```

Dette programmet vil sende en GET-forespørsel til "https://eksempel.no" med brukernavn og passord. Svaret vil være statuskoden for forespørselen.

## Dypdykk 

Å sende en HTTP-forespørsel med grunnleggende autentisering er ikke en ny oppfinnelse. Denne teknologien har vært tilgjengelig siden tidlige dager av world wide web for å hindre uautoriserte brukere i å få tilgang til hel eller delvis reservert informasjon.

På Rust-plattformen kan man også bruke `hyper` eller `actix-web` som alternativer til `reqwest`. Men `reqwest` er bredt akseptert som 'go-to'-valget på grunn av dets enkle syntaks og høye ytelse.

I et dypdykk i implementeringsdetaljer finner vi at HTTP Basic Auth bruker Base64-kodning. Dette er imidlertid ikke et kryptografisk sikkert system, så det er avgjørende å bruke HTTPS for å forsikre sikker overføring av brukerens legitimasjon.

## Se også

- Rust 'reqwest'-biblioteket: https://docs.rs/reqwest
- HTTP autentiseringsmetoder: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- 'hyper' i Rust: https://hyper.rs/
- 'actix-web' i Rust: https://actix.rs/