---
title:                "Opprette en midlertidig fil"
html_title:           "Rust: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å opprette en midlertidig fil er en vanlig praksis blant programmører for å lagre midlertidige data eller utføre midlertidige operasjoner under kjøring av et program. Dette kan være nyttig for å unngå å overskrive eller miste viktig informasjon på en permanent fil.

## Hvordan:

```Rust
use std::fs::File;
use std::io::prelude::*;

// Oppretter en ny midlertidig fil
let mut temp_file = File::create("midlertidig.txt").expect("Kunne ikke opprette midlertidig fil.");

// Skriver data til filen
temp_file.write_all(b"Dette er midlertidig data.").expect("Kunne ikke skrive til fil.");

// Leser data fra filen
let mut temp_data = String::new();
temp_file.read_to_string(&mut temp_data).expect("Kunne ikke lese data fra fil.");

// Sletter filen etter bruk
std::fs::remove_file("midlertidig.txt").expect("Kunne ikke slette midlertidig fil.");

println!("{}", temp_data);
```

Output:
```
Dette er midlertidig data.
```

## Dypdykk:

Opprettelsen av midlertidige filer har en lang historie og har blitt brukt i mange ulike programmeringsspråk gjennom årene. Et alternativ til å opprette en midlertidig fil er å bruke en buffert (buffer) i minnet, men dette kan føre til at det tar opp mer plass og dermed reduserer effektiviteten.

Implementeringen av opprettelse av midlertidige filer i Rust er enkel og intuitiv, da språket tilbyr innebygde funksjoner for å opprette, skrive til, lese fra og slette filer.

## Se også:

- Offisiell Rust dokumentasjon for å opprette filer: https://doc.rust-lang.org/std/fs/struct.File.html
- En detaljert guide til å opprette og håndtere filer i Rust: https://osblog.stephenmarz.com/chapter1.html