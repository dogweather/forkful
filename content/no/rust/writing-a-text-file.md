---
title:                "Rust: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan virke som en enkel oppgave, men det er faktisk en viktig del av programmering. Tekstfiler kan brukes til å lagre og lese data, og er et nødvendig trinn i mange programmeringsprosjekter. I denne bloggposten vil vi se nærmere på hvordan du kan skrive en tekstfil i Rust.

## Hvordan

For å skrive en tekstfil i Rust, må vi først importere "std::fs" biblioteket. Deretter kan vi bruke "File::create()" funksjonen for å opprette en ny fil, og deretter skrive data til den ved hjelp av "write_all()" funksjonen.

```Rust
use std::fs::File; // Importerer "std::fs" biblioteket
use std::io::Write; // Importerer "Write" traitet

fn main() {
    let mut fil = File::create("ny_fil.txt").expect("Kunne ikke opprette fil."); // Oppretter filen "ny_fil.txt"
    fil.write_all(b"Hei, dette er en tekstfil skrevet ved hjelp av Rust!").expect("Kunne ikke skrive til filen."); // Skriver data til filen
}
```

I koden over bruker vi "fil" variabelen til å holde styr på filen vår. Vi bruker også "mut" nøkkelordet for å gjøre den mutable, slik at vi kan skrive data til den. Deretter bruker vi "write_all()" funksjonen til å skrive en setning til filen. Vi legger også til "b" foran setningen for å indikere at den skal leses som bytes.

Når koden er kjørt, vil "ny_fil.txt" bli opprettet og teksten vår vil bli skrevet til den. Du kan åpne filen i en tekstredigerer for å se resultatet.

## Dypdykk

I "How To" delen viste vi hvordan du kan skrive en enkel tekstfil, men det er også mulig å skrive mer komplekse filer ved hjelp av Rust. Du kan for eksempel bruke "File::open()" funksjonen til å åpne en eksisterende fil og deretter bruke "read_to_string()" funksjonen for å lese data fra filen.

```Rust
use std::fs::File; // Importerer "std::fs" biblioteket
use std::io::Read; // Importerer "Read" traitet

fn main() {
    let mut fil = File::open("ny_fil.txt").expect("Kunne ikke åpne fil."); // Åpner filen "ny_fil.txt"
    let mut data = String::new(); // Oppretter en tom streng
    fil.read_to_string(&mut data).expect("Kunne ikke lese filen."); // Leser data fra filen og legger til det til strengen
    println!("{}", data); // Skriver ut data til konsollen
}
```

I koden over bruker vi "File::open()" funksjonen til å åpne filen vår og lagre den i "fil" variabelen. Deretter oppretter vi en tom streng og bruker "read_to_string()" funksjonen til å lese data fra filen og legge den til i strengen. Til slutt skriver vi ut dataen til konsollen.

## Se også

- [Offisiell Rust dokumentasjon](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust By Example guide om filbehandling](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file/read_lines.html)
- [En tutorial om filbehandling i Rust](https://link.medium.com/1cNbWmv8N0)