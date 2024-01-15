---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Rust: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor 

Det er mange grunner til å sende et HTTP-anrop med basic autentisering. Kanskje trenger du tilgang til et API som krever autentisering for å hente data, eller kanskje vil du sende informasjon til en sikker server.

## Hvordan

For å sende et HTTP-anrop med basic autentisering i Rust må du følge disse trinnene:

1. Importer `reqwest` biblioteket ved å legge til følgende linje i `Cargo.toml` filen din:

   ``` rust
   [dependencies]
   reqwest = { version = "0.11", features = ["json", "blocking"] }
   ```

   **Merk:** dette eksempelet bruker versjonen 0.11 av `reqwest`, men versjon 0.10 eller høyere skal også fungere.

2. I `main.rs` filen din, legg til `use reqwest::header::{Authorization, Basic};` for å importere nødvendige biblioteker.

3. Definer variabler for å holde URLen du vil sende anropet til og autentiseringsinformasjonen din:

   ``` rust
   let url = "https://example.com/api/data";
   let username = "brukernavn";
   let password = "passord";
   ```

4. Bygg autentiseringsinformasjonen ved å lage en `Basic` struct og legge til brukernavn og passord:

   ``` rust
   let auth = Basic {
       username,
       password: Some(password),
   };
   ```

5. Opprett en HTTP klient og legg til autentiseringsinformasjonen din med metoden `basic_auth`:

   ``` rust
   let client = reqwest::blocking::Client::new();
   let response = client
       .get(url)
       .basic_auth(username, Some(password))
       .send();
   ```

6. Hvis alt går bra, vil du få en respons tilbake i form av en `Response` struct. Du kan få tilgang til svaret ved hjelp av metoden `text` og skrive det ut:

   ``` rust
   let body = response.text().unwrap();
   println!("Svaret er:{}", body);
   ```

## Dypdykk

Hvis du ønsker å utforske mer om sending av HTTP-anrop med basic autentisering, kan du se på dokumentasjonen til `reqwest` biblioteket og lese om forskjellige metoder og funksjoner som er tilgjengelige.

For eksempel kan du bruke `header` metoden for å legge til egendefinerte headere i HTTP-anropet ditt, eller bruke `json` metoden for å sende og motta JSON-data.

## Se også

- [Dokumentasjon for `reqwest` biblioteket] (https://docs.rs/reqwest/0.11.0/reqwest/)
- [Offisiell Rust nettside] (https://www.rust-lang.org/)

Dette var en enkel introduksjon til hvordan du sender et HTTP-anrop med basic autentisering i Rust. Jeg håper dette har vært nyttig for deg og at du nå er klar til å implementere det i dine egne prosjekter. Lykke til!