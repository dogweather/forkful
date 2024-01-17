---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Rust: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP forespørsel med grunnleggende autentisering betyr å inkludere brukernavn og passord i en forespørsel til en webside eller tjeneste. Dette gjøres for å sikre at kun autoriserte brukere får tilgang til informasjonen som blir forespurt. Programmere bruker dette for å legge til sikkerhet i sine applikasjoner.

## Hvordan:
Kodingseksempel:
```Rust
let username = "brukernavn";
let password = "passord";

let auth = base64::encode(format!("{}:{}", username, password));

let client = reqwest::Client::new();
let resp = client
    .get("https://nettside.com/api")
    .header("Authorization", format!("Basic {}", auth))
    .send()
    .await?;
```
Eksempel på resultat:
```
Status: 200 OK
Body: {"username": "brukernavn", "message": "Hei, velkommen!"}
```

## Dykk Dypere:
Historisk kontekst: Grunnleggende autentisering har vært en vanlig måte å legitimere seg på siden HTTP ble utviklet på 1990-tallet. I dag er det fortsatt mye brukt, men det finnes også alternative autentiseringsmetoder som OAuth og API-tokens.

Alternativer: Som nevnt ovenfor, er OAuth og API-tokens populære alternativer til grunnleggende autentisering når det kommer til å sikre websider og applikasjoner.

Implementasjonsdetaljer: Når man sender en HTTP forespørsel med grunnleggende autentisering, må brukernavnet og passordet sendes kryptert i Base64-format i headeren til forespørselen.

## Se Også:
- [Rust Offisiell Nettside](https://www.rust-lang.org/)
- [Rust Dokumentasjon](https://doc.rust-lang.org/)
- [Base64 Rust Bibliotek](https://crates.io/crates/base64)
- [Reqwest Rust Bibliotek](https://crates.io/crates/reqwest)