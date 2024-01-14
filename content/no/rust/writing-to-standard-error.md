---
title:    "Rust: Skriving til standard feil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard feil er en viktig del av å lage pålitelige og robuste Rust-programmer. Når du skriver til standard feil, er det en effektiv måte å håndtere feil og uventede situasjoner som kan oppstå i løpet av kjøretiden til programmet ditt.

## Hvordan

Rust har en innebygd funksjon for å skrive til standard feil, som kalles `eprintln!()` . Denne funksjonen tar inn en formateringsstreng og eventuelle variabler som du vil skrive ut, og skriver dem til standard feil i stedet for standard utgang.

Her er et eksempel på hvordan du kan bruke `eprintln!()` for å skrive ut en feilmelding hvis en fil ikke kan åpnes:

```Rust
use std::fs::File;

let file = File::open("my_file.txt");

match file {
    Ok(f) => {
        // gjør noe med filen her
    },
    Err(e) => {
        // skriv feilmelding til standard feil
        eprintln!("Kunne ikke åpne filen: {}", e);
    }
}
```

Når programmet ditt kjører, vil feilmeldingen bli skrevet til standard feil i stedet for å bli vist på skjermen. Dette gjør det enklere å fange opp potensielle problemer og håndtere dem på en hensiktsmessig måte.

## Dypdykk

Å skrive til standard feil kan også være nyttig når du vil logge informasjon om programmet ditt under kjøring. Ved å skrive til standard feil i stedet for standard utgang, kan du få en tydeligere og mer organisert utskrift av dataene dine.

I tillegg kan du også bruke `eprintln!()` for å logge informasjon om debug-utskrifter mens du utvikler og tester programmet ditt. Dette er spesielt nyttig hvis du jobber med flertrådede applikasjoner eller komplekse systemer.

## Se også

- [Rust dokumentasjon: eprintln!()](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Rust referanse: std::io::stderr()](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Rust guide: håndtering av feil](https://doc.rust-lang.org/book/ch09-00-error-handling.html)