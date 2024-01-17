---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Rust: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en mappe eksisterer er å sjekke om en bestemt sti på datamaskinen din faktisk fører til en mappe eller ikke. Dette er en nyttig funksjon for å sikre at programmet ditt kan finne og lese de nødvendige filene det trenger.

## Hvordan:
Å sjekke om en mappe eksisterer i Rust er enkelt og gjøres ved hjelp av funksjonen ```std::path::Path::exists()```. Her er et eksempel:

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");

    if path.exists() {
        println!("Mappen eksisterer!");
    } else {
        println!("Mappen eksisterer ikke.");
    }
}
```

Dette vil skrive ut enten "Mappen eksisterer!" eller "Mappen eksisterer ikke.", avhengig av om mappen finnes eller ikke.

## Dykk dypere:
Det å sjekke om en mappe eksisterer er noe som ofte gjøres i programmering for å sikre at programmet kan finne og bruke de nødvendige filene det trenger. Alternativt kan man også bruke ```std::fs::metadata()``` for å få mer detaljert informasjon om en gitt fil eller mappe.

Hvis du bruker en eldre versjon av Rust, kan du også bruke ```std::fs::Path::is_dir()``` for å sjekke om en sti peker til en mappe.

Når du sjekker om en mappe eksisterer, blir det faktisk gjort en systemkall til operativsystemet, så det kan være litt tregt hvis du gjør dette mange ganger i løpet av programmet ditt.

## Se også:
- [Rust dokumetasjon for std::path::Path::exists()](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)
- [Rust dokumetasjon for std::fs::metadata()](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Rust dokumetasjon for std::fs::Path::is_dir()](https://doc.rust-lang.org/std/fs/struct.Path.html#method.is_dir)