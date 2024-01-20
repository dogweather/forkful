---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Rust programmering: Sende en HTTP-forespørsel

## Hva & Hvorfor?

En HTTP-forespørsel er en protokoll som brukes for å hente eller sende data over nettet. I programmering, brukes dette til å samhandle med andre webtjenester eller APIs.

## Hvordan det fungerer:

Vi skal bruke `reqwest` biblioteket i Rust for å sende en GET forespørsel. Først legger vi til `reqwest` avhengighet i `Cargo.toml` filen:

```Rust
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Deretter, vi skriver en enkel GET forespørsel i vår `main.rs`:

```Rust
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let res = reqwest::get("https://httpbin.org/get").await?;
    let body = res.text().await?;
    println!("Body:\n{}", body);
    Ok(())
}
```
Om alt gikk etter planen, bør du nå se en utskrift av responsen fra httpbin.org i konsollen.

## Dypdykk

HTTP-forespørsler kom inn i bruk tidlig i internettets historie, og har vært en kritisk komponent i hvordan data blir sendt og mottatt. Alternativt kan noen bruker lavere nivå biblioteker som `hyper` for mer tilpasning, men `reqwest` tilbyr en mer høy-nivå og enklere å bruke API for de fleste.

Når det kommer til implementeringsdetaljer, bruker `reqwest` biblioteket `tokio` for asynkronitet. Dette betyr at forespørsler ikke vil blokkere din applikasjon mens du venter på svar fra serveren.

## Se Også

1. Rust dokumentasjon: [reqwest](https://docs.rs/reqwest/0.11.6/reqwest/)
2. Offisiell Rust bok: [Asynkron Programmering](https://rust-lang.github.io/async-book/)
3. HTTP-forespørsler i Rust: [En grundig introduksjon og guide](https://www.section.io/engineering-education/http-requests-in-rust/)