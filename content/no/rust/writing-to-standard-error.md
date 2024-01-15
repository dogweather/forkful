---
title:                "Skriver til standardfeil"
html_title:           "Rust: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor Skrive Til Standardfeil I Rust?

Å skrive til standardfeil i Rust er en viktig måte å kommunisere feil og advarsler til brukeren av et program. Når man skriver en Rust-applikasjon, er det viktig å gi tydelig og pålitelig informasjon hvis noe går galt. Ved å skrive til standardfeil, kan man sikre at brukeren får nødvendig informasjon for å kunne løse eventuelle problemer som oppstår.

## Hvordan Gjøre Det

Det er enkelt å skrive til standardfeil i Rust ved å bruke standardbiblioteket `std::io`. Her er et enkelt eksempel på hvordan man kan skrive en feilmelding til standardfeil:

```rust
fn main() {
    let err_msg = "Noe gikk galt!";
    eprintln!("{}", err_msg);
}
```

Dette vil skrive ut følgende melding til standardfeil:

```
Noe gikk galt!
```

Man kan også bruke `panic!` makroen for å skrive til standardfeil og avslutte programmet med en feilmelding:

```rust
fn main() {
    let err_msg = "Noe gikk galt!";
    panic!("{}", err_msg);
}
```

I tillegg til å skrive feilmeldinger, kan man også skrive advarsler til standardfeil ved å bruke `eprintln!` makroen.

## Dypdykk

Når man skriver til standardfeil, bruker man vanligvis `eprintln!` makroen i stedet for `println!` makroen. Dette er fordi `eprintln!` automatisk legger til en ny linje på slutten av meldingen, mens `println!` ikke gjør det. Dette kan føre til uklare utskrifter hvis man skriver flere meldinger etter hverandre. Her er et eksempel som viser forskjellen:

```rust
fn main() {
    let err_msg = "Noe gikk galt!";
    eprintln!("Dette er første feilmelding.");
    eprintln!("{} Dette er andre feilmelding.", err_msg);
}
```

Dette vil skrive ut følgende til standardfeil:

```
Dette er første feilmelding.
Noe gikk galt! Dette er andre feilmelding.
```

Hvis man bruker `println!` i stedet, vil meldingene skrives ut på samme linje, noe som kan være forvirrende for brukeren.

Det er også verdt å merke seg at man kan skrive større og mer detaljerte meldinger til standardfeil ved å bruke `format!` og `eprintln!` sammen. Her er et eksempel:

```rust
fn main() {
    let err_code = 404;
    let err_msg = "Ressursen ble ikke funnet.";
    eprintln!("Error {}: {}", err_code, err_msg);
}
```

Dette vil skrive ut følgende melding til standardfeil:

```
Error 404: Ressursen ble ikke funnet.
```

# Se Også

- [The `std::io` module in the Rust Standard Library](https://doc.rust-lang.org/std/io/index.html)
- [Rust Error Handling](https://www.rust-lang.org/learn/error-handling-in-rust)