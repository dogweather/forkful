---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside betyr å hente innholdet på en nettside inn på din egen datamaskin. Programmerere gjør dette for å analysere eller manipulere dataene til senere bruk.

## Hvordan:

Her er et grunnleggende eksempel i Rust på hvordan du kan laste ned en nettside ved hjelp av `reqwest`, et populært http klient bibliotek for Rust.

```Rust
use reqwest::Error;

async fn hent_web_side() -> Result<(), Error> {
   let innhold = reqwest::get("https://www.example.com")
        .await?
        .text()
        .await?;
    println!("Innhold: {}", innhold);
    Ok(())
}
```

For å kjøre funksjonen ovenfor bruker du:

```Rust
fn main() {
    let result = tokio::runtime::Runtime::new()
        .unwrap()
        .block_on(hent_web_side());
    
    match result {
        Ok(_) => println!("Suksess!"),
        Err(e) => println!("Noe gikk galt: {}", e),
    }
}
```

## Dypdykk:

Mens vi utforsker hvordan du henter innholdet på en nettside, kan vi også utforske litt historie, alternative metoder og detaljer ved implementeringen.

1. Historisk kontekst: Opprinnelig ble nettinnhold bare sett i nettlesere, men med fremveksten av web scraping, har anvendelsene av dette vokst.

2. Alternativer: Det finnes mange biblioteker for å laste ned websider i Rust, som hyper og surf.

3. Implementeringsdetaljer: `reqwest` er basert på hyper library, og `tokio` brukes til å håndtere asynkroniteten i Rust. Det er viktig å behandle feilene riktig, og det er der `Result` typen kommer inn i bildet, som viser enten suksess eller feil.

## Se også:

- [Reqwest Dokumentasjon](https://docs.rs/reqwest)
- [Hyper Dokumentasjon](https://hyper.rs/)
- [Surf Dokumentasjon](https://docs.rs/surf)
- [Dybde i feilhåndtering i Rust](https://blog.burntsushi.net/rust-error-handling/)