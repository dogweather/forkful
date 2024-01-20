---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive til en tekstfil betyr å lagre data i en fil på disk. Vi gjør dette for å bevare informasjon mellom økter og dele data mellom forskjellige programmer eller brukere.

## Slik gjør du:

Rust bruker `std::fs` modulen til å håndtere filer. Her er et enkelt eksempel:

```rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut fil = File::create("eksempel.txt")?;
    fil.write_all(b"Hei, verden!")?;
    Ok(())
}
```

Når du kjører dette, lager du en fil med navn `eksempel.txt` med innholdet "Hei, verden!".

## Deep Dive

Å skrive til tekstfiler er grunnleggende og så gammelt som datamaskiner selv. I Rust, er `std::fs::File` og `std::io::Write` høy-nivå abstraksjoner som gjør filoperasjoner enklere. Alternativer inkluderer lav-nivå systemkall via `libc` eller asynkrone biblioteker som `tokio`. Når du skriver til en fil, må du passe på feilhåndtering, tilgangskontroll og datakonkurranser.

## Se Også

- Rust sin offisielle dokumentasjon for `std::fs`: https://doc.rust-lang.org/std/fs/index.html
- Rust by Example - Fil I/O: https://doc.rust-lang.org/rust-by-example/std_misc/file.html
- Rust boken: https://doc.rust-lang.org/book/