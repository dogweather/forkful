---
title:    "Rust: Slette tegn som matcher et mønster"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Hvorfor

I Rust-programmeringsspråket er det ofte nødvendig å jobbe med tekststrenger og utføre operasjoner på dem, som for eksempel å slette bestemte deler av en streng. En vanlig oppgave er å slette alle tegn som matcher et bestemt mønster, og dette kan være nyttig for å rengjøre og formatere data. I denne artikkelen vil vi gå gjennom hvordan man kan slette tegn som matcher et mønster ved hjelp av Rust.

# Hvordan gjøre det

For å slette tegn som matcher et mønster i Rust, trenger vi å bruke et konsept som kalles "regex" (regulære uttrykk). Regex er et kraftig verktøy for å matche og manipulere tekststrenger, og Rust har et eget bibliotek kalt "regex" som gjør det enkelt å jobbe med dette.

La oss si at vi har en streng som inneholder et telefonnummer med følgende format: "(xxx) xxx-xxxx". Hvis vi ønsker å slette alle parentesene og bindestreken for å få et renere telefonnummer, kan vi bruke følgende kode:

```Rust
use regex::Regex;

let phone_number = "(123) 456-7890";

let regex = Regex::new(r"[\(\)\-]").unwrap(); 
// Her definerer vi et regex-mønster som inneholder parenteser og bindestrek
// Vi bruker "\\(" og "\\)" for å unngå at Rust tolker disse som spesialtegn

let cleaned_number = regex.replace_all(phone_number, "");
// Her bruker vi "replace_all" funksjonen til å erstatte alle tegn som matcher regex-mønsteret med ingenting
```

Etter å ha kjørt koden vil "cleaned_number" få verdien "1234567890", uten parenteser og bindestrek.

# Dypdykk

Du lurer kanskje på hva som skjer bak kulissene når vi bruker "regex" i Rust. Regex fungerer ved å tolke mønstrene som et uttrykk og deretter sammenligne dette med den gitte tekststrengen. Det finnes forskjellige muligheter for å definere regex-mønstre, avhengig av kompleksiteten og behovet for dine operasjoner. For å lære mer om dette kan du ta en titt på Rusts offisielle dokumentasjon for "regex" biblioteket.

# Se også

Her er noen nyttige ressurser for å lære mer om hvordan man kan slette tegn som matcher et mønster i Rust:
- [Offisiell Rust dokumentasjon for "regex" biblioteket](https://docs.rs/regex/1.4.2/regex/)
- [Utforsk mer avanserte regex-mønstre med Rust på Rust Playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=8237917fdd746cec1df546e7e8b3c159)
- [Sjekk ut Rust By Example for et praktisk eksempel på regex-bruk](https://rustbyexample.com/std/regex.html)