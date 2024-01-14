---
title:                "Rust: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Hvorfor

Når du jobber med programmering, kan du ofte komme over situasjoner der du må søke og erstatte bestemte tekster i koden din. Dette kan være en tidkrevende og kjedelig oppgave hvis du må gjøre det manuelt. Heldigvis er det en enkel og effektiv løsning for dette problemet i Rust programmeringsspråk.

##Slik gjør du det

I Rust har vi en innebygd funksjon som heter `replace`. Denne funksjonen lar deg søke etter et bestemt tekststykke og erstatte det med en annen tekst. For å bruke denne funksjonen, må du først importere `replace` funksjonen fra `std:: string` biblioteket. Deretter kan du bruke den i koden din som følger:

```Rust
use std::string::replace;

let original_text = "Hei, dette er en tekststreng for eksempel";
let ny_text = replace(original_text, "for eksempel", "for illustrasjon");

println!("Original tekst: {}", original_text);
println!("Ny tekst: {}", ny_text);

```

Når du kjører denne koden vil utgangen bli:

```
Original tekst: Hei, dette er en tekststreng for eksempel
Ny tekst: Hei, dette er en tekststreng for illustrasjon
```

Som du kan se, erstattet funksjonen `replace` den opprinnelige teksten med den nye teksten du angav. Du kan også bruke denne funksjonen på en variabel istedenfor en fast tekst.

##Dypdykk

Hvis du ønsker å utføre mer avanserte søk og erstatningsoperasjoner, har Rust et annet alternativ som kan være nyttig for deg - `regex` biblioteket. Dette biblioteket lar deg bruke regulære uttrykk for å søke og erstatte tekst. For å bruke dette biblioteket, må du først importere det i koden din som følger:

```Rust
use regex::Regex;
```

Deretter kan du bruke `Regex` struct for å konstruere et regulært uttrykk og deretter bruke `replace_all` metoden for å søke og erstatte teksten din. Et eksempel kan se slik ut:

```Rust
let original_text = "Hei, dette er en tekststreng for eksempel";
let regex = Regex::new(r"for eksempel").unwrap();
let ny_text = regex.replace_all(&original_text, "for illustrasjon");

println!("Original tekst: {}", original_text);
println!("Ny tekst: {}", ny_text);
```

Denne koden vil gi samme resultat som i forrige eksempel, men gir deg også muligheten til å utføre mer avanserte søk og erstatningsoperasjoner ved hjelp av regulære uttrykk.

##Se Også

- [Rust Dokumentasjon om Søk og Erstatt Operasjoner](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Regex Dokumentasjon for Rust](https://docs.rs/regex/1.4.2/regex/)
- [En Gjennomgang av Regulære Uttrykk i Rust](https://medium.com/quick-code/regex-in-rust-4beb1b723e80)