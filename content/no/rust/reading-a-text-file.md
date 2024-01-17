---
title:                "Leser en tekstfil"
html_title:           "Rust: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av en tekstfil er en vanlig operasjon i programmering der man henter informasjon fra en tekstfil og behandler den på en eller annen måte. Dette gjøres vanligvis for å få tilgang til data som er lagret i en tekstfil eller for å behandle store mengder data på en effektiv måte.

## Hvordan:
```Rust
use std::fs::File;

// Åpner en tekstfil og leser innholdet som en String
let mut file = File::open("tekstfil.txt").expect("Kunne ikke åpne fil");
let mut contents = String::new();
file.read_to_string(&mut contents).expect("Kunne ikke lese fil");

// Skriver ut innholdet i filen
println!("{}", contents);
```

For å lese en tekstfil, må vi først åpne filen ved å bruke `File::open` funksjonen fra standardbiblioteket `std::fs`. Deretter oppretter vi en variabel for å lagre filinnholdet og bruker `read_to_string` funksjonen for å lese innholdet som en `String`. Til slutt skriver vi ut innholdet ved å bruke `println!` makroen.

## Dypdykk:
Lesing av tekstfiler har vært en del av programmering siden de første programmeringsspråkene ble utviklet. I dag finnes det mange alternative måter å lese tekstfiler på, for eksempel ved å bruke `io::BufReader` fra Rusts standardbibliotek eller ved å bruke tredjepartsbiblioteker som `serde` for å lese innholdet og konvertere det til andre datatyper.

Når man leser en tekstfil i Rust, vil filinnholdet bli lagret som en `String`. Dette kan føre til problemer hvis filen er veldig stor, da dette kan føre til at programmet bruker mye minne. Derfor kan det være lurt å vurdere alternative måter å lese filen på, avhengig av hva slags informasjon man ønsker å hente ut fra filen.

## Se også:
- https://doc.rust-lang.org/std/fs/struct.File.html
- https://doc.rust-lang.org/std/io/struct.BufReader.html
- https://serde.rs/