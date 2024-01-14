---
title:    "Rust: Søke og erstatte tekst"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

For de som jobber med programmering og tekstbehandling, er det å søke og bytte ut tekst en viktig oppgave. Dette kan spare tid og redusere risikoen for feil i koden. I denne bloggposten vil vi utforske hvordan dette kan gjøres ved hjelp av Rust-programmeringsspråket.

## Hvordan

For å søke og erstatte tekst i en fil ved hjelp av Rust, kan du bruke standardbiblioteket Regex. Først må vi legge til "regex"-pakken i dependencies-seksjonen i Rust-prosjektet vårt. Deretter kan vi importere regex-biblioteket og bruke dets funksjoner.

```Rust
// Importerer Regex-biblioteket
use regex::Regex;

// Definerer den ønskede teksten som skal erstattes
let text = "Hei, dette er en testtekst";

// Definerer et Regex-objekt som skal søke etter tekst som matcher uttrykket vårt
let regex = Regex::new(r"testtekst").unwrap();

// Utfører søket og erstatter den matchede teksten med "eksempeltekst"
let replaced_text = regex.replace_all(text, "eksempeltekst");

// Skriver ut resultatet
println!("{}", replaced_text); // Hei, dette er en eksempeltekst
```

Som du kan se, bruker vi her metoden `replace_all` for å erstatte all forekomst av søketeksten med det ønskede resultatet.

## Deep Dive

Regex-biblioteket i Rust har mange nyttige funksjoner som gjør det enkelt å søke og erstatte tekst. Det støtter blant annet regulære uttrykk, som gjør det mulig å søke etter mer avanserte mønstre i teksten. Du kan også bruke `replace`-metoden for å erstatte kun den første forekomsten av søketeksten i en tekst.

Det er også verdt å merke seg at Regex-biblioteket har god støtte for Unicode, noe som gjør det enkelt å håndtere forskjellige språk og tegnsett i teksten.

## Se også

- [Rust Regex-dokumentasjon](https://docs.rs/regex/1.4.2/regex/)
- [Regulære uttrykk i Rust](https://blog.burntsushi.net/transducers/#a-tutorial-on-regular-expressions)
- [Unicode-støtte i Rust Regex-biblioteket](https://www.rust-lang.org/learn/get-started#unicode-support-no-encodings-required)