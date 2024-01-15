---
title:                "Å lese en tekstfil"
html_title:           "Rust: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan lese tekstfiler i en Rust-applikasjon? Fortvil ikke, det er faktisk en enkel prosess!

## Hvordan

```Rust
use std::fs;

fn main() {
    let file = fs::read_to_string("filnavn.txt")
        .expect("Kunne ikke lese filen");
    
    println!("Innholdet i filen er:\n{}", file);
}
```

Output:
```Rust
Innholdet i filen er:
Dette er en tekstfil som skal leses i en Rust-applikasjon.
```

## Dypdykk

For å lese en tekstfil i Rust, bruker vi funksjonen `read_to_string`, som returnerer en `String` som inneholder hele filens innhold. Denne funksjonen tar inn filnavnet som parameter og håndterer eventuelle feil, som vi ser i eksempelet over.

Det er også mulig å lese en fil linje for linje ved hjelp av en iterator. Et eksempel på dette kan være:

```Rust
use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let file = File::open("filnavn.txt").expect("Kunne ikke åpne filen");
    let reader = BufReader::new(file);
    for line in reader.lines() {
        println!("{}", line.unwrap());
    }
}
```

Dette vil skrive ut hver linje i filen separat.

## Se også

- [Rust dokumentasjon](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Rust lærebøker](https://www.rust-lang.org/learn)
- [Rust forum](https://users.rust-lang.org/)