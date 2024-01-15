---
title:                "Skriving av en tekstfil"
html_title:           "Rust: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle du bry deg med å skrive en tekstfil i Rust? Svaret er enkelt: for å lagre og organisere data på en strukturert måte som kan brukes av datamaskinen din. Enten det er et enkelt notat eller et komplekst dataprogram, å kunne skrive til en tekstfil er en viktig ferdighet for enhver programmerer.

## Slik

Skrive til en tekstfil i Rust er enkelt og rett frem. Først må du inkludere "std" biblioteket ved å skrive `use std::fs::File;` øverst i koden din. Deretter kan du bruke `File::create` funksjonen til å opprette en ny tekstfil og få en `File`-objekt som representerer filen. Her er et eksempel på å skrive tekst inn i filen:

```Rust
use std::fs::File;

let mut file = File::create("min_fil.txt")?; // Oppretter en fil med navnet "min_fil.txt"
file.write_all(b"Hei, verden!")?; // Skriver til filen
```

Dette vil skrive teksten "Hei, verden!" til tekstfilen "min_fil.txt". Legg merke til `?` etter funksjonene. Dette er Rusts måte å håndtere eventuelle feil på. Du kan også bruke `writeln!` makroen for å skrive til filen og legge til linjeskift automatisk:

```Rust
use std::fs::File;
use std::io::Write; // Trenger for å bruke writeln! makroen

let mut file = File::create("min_fil.txt")?;
writeln!(file, "Dette er en linje")?; // Skriver "Dette er en linje" og legger til linjeskift
```

## Dypdykk

Det er flere detaljer du kan lære for å utvide dine ferdigheter i å skrive til en tekstfil i Rust. En av dem er å åpne en eksisterende tekstfil og lese data fra den. Du kan også bruke forskjellige formateringsmakroer som `format!` og `println!` for å formatere data før du skriver dem til filen. Det er også nyttig å lære mer om håndtering av feil og unntak når du arbeider med filer.

## Se også

- [Rust Dokumentasjon om filer](https://doc.rust-lang.org/std/fs/index.html)
- [Rust Tutorial: File I/O](https://www.tutorialspoint.com/rust/rust_file_io.htm)
- [Rust Book - Kapittel 12: Anor de konsert](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)