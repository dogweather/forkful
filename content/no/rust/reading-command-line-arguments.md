---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Rust: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Lesing av kommandolinjeargumenter er en måte for programmerere å få tilgang til og bruke informasjon som brukeren skriver inn direkte fra kommandolinjen. Dette kan være nyttig for å tilpasse programmet eller utføre spesifikke handlinger basert på brukerens input.

# Hvordan:
```rust
use std::env;

fn main() {
    // Hente kommandolinjeargumentene og lagre dem som en vektor
    let args: Vec<String> = env::args().collect();

    // Skrive ut alle argumentene
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

Eksempel på output:
```bash
$ rustc args.rs
$ ./args arg1 arg2 arg3
arg1
arg2
arg3
```

# Dypdykk:
(1) Reading kommandolinjeargumenter er en vanlig praksis i programmering og stammer fra tiden da datamaskiner ble betjent via terminaler.
(2) En alternativ måte å få tak i brukerinput på er via standard input-strømmen.
(3) Rusts standardbibliotek har en modul som heter "env" som inneholder funksjoner for å hente informasjon fra brukerens miljø, for eksempel kommandolinjeargumenter.

# Se også:
- Rust dokumentasjon for "env" modul: [https://doc.rust-lang.org/std/env/index.html]
- En artikkel om å hente input ved hjelp av standard input-strømmen: [https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html]