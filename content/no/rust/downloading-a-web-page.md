---
title:                "Nedlasting av en nettside"
html_title:           "Rust: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til hvorfor du skulle ønske å laste ned nettsider, enten det er for å lagre informasjon, hente data eller for å automatisere en prosess. Uansett hva grunnen måtte være, så har Rust et kraftig bibliotek som gjør det enkelt og effektivt å laste ned nettsider.

## Hvordan gjøre det
For å laste ned en nettside med Rust, trenger du først å importere biblioteket "reqwest". Deretter kan du bruke følgende kode for å laste ned en nettside og skrive ut responsen:

```rust
use reqwest;

fn main() {
    let mut response = reqwest::get("https://www.example.com").expect("Failed to get response");

    println!("Response status code: {}", response.status());

    let body = response.text().expect("Failed to read response body");
    println!("Response body: {}", body);
}
```

Koden over vil først hente responsen fra nettsiden og deretter skrive ut statuskoden og innholdet på nettsiden. Dette er en enkel måte å laste ned en nettside på, men det finnes mange flere funksjoner og muligheter i "reqwest" biblioteket.

## Dypdykk
For de som er mer erfarne i Rust og ønsker å utforske mer avanserte muligheter for å laste ned nettsider, så er det flere alternativer tilgjengelig. Du kan for eksempel spesifisere en brukeragent, håndtere omdirigeringer eller legge til egendefinerte HTTP-headerfelter i forespørselen.

Det er også mulig å konfigurere "reqwest" for å bruke en HTTP-proxy eller aktivere HTTPS-verifisering. Det finnes også alternative biblioteker som gir mer fleksibilitet og kontroll over HTTP-forespørsler, som "hyper" og "curl".

## Se også
- [Offisiell "reqwest" dokumentasjon](https://docs.rs/reqwest/0.11.1/reqwest/)
- [Eksempelprosjekter på GitHub](https://github.com/search?q=reqwest+rust)