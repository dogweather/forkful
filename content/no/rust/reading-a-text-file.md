---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesing av tekstfiler i Rust

## Hva & Hvorfor?
Å lese en tekstfil betyr å hente innholdet i filen for manipulering eller visning. Programmerere gjør dette for å behandle data, konfigurere systemer, og for mange andre nytteverdige formål.

## Hvordan Gjør Jeg Det:
Å lese en tekstfil i Rust er ganske rett frem. Her er enkel kode for å gjøre det:

```Rust
use std::fs;

fn main() {
    let innholdet = fs::read_to_string("sti/til/din/fil.txt")
        .expect("Kunne ikke lese filen.");

    println!("Innholdet i filen er: \n{}", innholdet);
}
```
Kjører du denne koden på en fil som inneholder teksten "Hei, verden!", får du følgende utskrift:

```Rust
Innholdet i filen er: 
Hei, verden!
```

## Dybdeplunging:
Historisk har innLesing av filer vært sentralt i datasystemer. For eksempel kom UNIX med kommandoen 'cat' allerede i 1971 for å lese filinnhold.

Alternativt kan Rust også lese filer linje for linje ved bruk av `BufRead` og `lines()`. Denne metoden er nyttig når du arbeider med veldig store filer:

```Rust
use std::io::{self, BufRead};
use std::fs::File;

fn main() -> io::Result<()> {
    let file = File::open("sti/til/din/fil.txt")?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }
    Ok(())
}
```
Rusts `std::fs` og `std::io` moduler gir enkel og robust tilgang til filsystemfunksjoner. Å optimalisere for leseytelse eller minnebruk vil avhenge av dine spesifikke behov.

## Se Også:
1. Rusts offisielle dokumentasjon for [`std::fs`](https://doc.rust-lang.org/std/fs/index.html) og [`std::io`](https://doc.rust-lang.org/std/io/index.html).
2. En mer omfattende guide om filbehandling i Rust på [Rust Cookbook](https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html).
3. Diskusjon om fillesing i Rust på [Stack Overflow](https://stackoverflow.com/questions/31192956/whats-the-de-facto-way-of-reading-and-writing-files-in-rust-1-x).