---
title:                "Å bruke regulære uttrykk"
html_title:           "Rust: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions er et uttrykk brukt til å beskrive mønstre innenfor en tekst eller en streng av tegn. Det kan brukes til å søke, erstatte og manipulere tekst på en effektiv måte. Programmere bruker det for å utføre avansert tekstbehandling, validering av brukerinput og databehandling.

## Hvordan:
La oss si at vi har en liste over e-postadresser og vi ønsker å filtrere ut alle adressene som ender på "gmail.com". Vi kan bruke Rusts regex biblotek for å oppnå dette:

```Rust
use regex::Regex;

fn main() {
    let email_list = ["john@test.com", "sarah@test.com", "mark@gmail.com", "jane@gmail.com"];
    let gmail_regex = Regex::new(r"gmail.com$").unwrap();
    
    for email in email_list {
        if gmail_regex.is_match(email) {
            println!("{}", email);
        }
    }
}
```

Dette vil gi følgende utskrift:
```
mark@gmail.com
jane@gmail.com
```

## Dykke dypere:
Regular expressions har eksistert i mange år og har blitt brukt i mange programmeringsspråk. Alternativene inkluderer Perl, Python og JavaScript. I Rust finnes det mange tredjepartsbiblioteker som tilbyr regex-funksjonalitet, men standardbiblioteket inkluderer nå et eget regex-modul.

Implementeringen av regex i Rust er basert på NFA (Non-Deterministic Finite Automaton) og brukes for å bygge et state machine for å matche og søke gjennom tekst. Rusts regex-bibliotek er også utfordret til å være en av de raskeste sammenlignet med andre programmeringsspråk.

## Se også:
- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rust Regex Dokumentasjon](https://docs.rs/regex/)
- [Rust Regex Markedsplace](https://crates.io/keywords/regex)