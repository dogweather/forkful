---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Lua: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Rust programmering: Hvordan sjekke om en mappe eksisterer

## Hva & Hvorfor?
I programmering betyr 'å sjekke om en mappe eksisterer' å bekrefte om en bestemt filsti peker mot en eksisterende mappe. Dette gjøres for å unngå errors når vi prøver å manipulere filer eller mapper som kanskje ikke finnes.

## Hvordan gjøre det:
Du kan bruke `std::path::Path` og metoden `exists()` for å sjekke om en mappe eksisterer i Rust.

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/noen/mappe/sti");

    if path.exists() {
        println!("Mappen eksisterer");
    } else {
        println!("Mappen eksisterer ikke");
    }
}
```
Hvis mappen eksisterer, vil "Mappen eksisterer" printes ut. Ellers vil "Mappen eksisterer ikke" printes ut.

## Deep Dive
Sjekke om en mappe eksisterer er ikke en ny idé; det har vært en nødvendig del av programmering siden de tidligste dager. Før Rust, kunne du gjøre dette i C med `stat()` funksjonen, for eksempel.

En annen metode i Rust du kan bruke til å sjekke om en filsti peker til en eksisterende mappe er `metadata()`. Denne metoden returnerer mer detaljerte data om filsti, inkludert om det er en mappe eller ikke.

```Rust
use std::fs;

fn main() {
    match fs::metadata("/noen/mappe/sti") {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Dette er en mappe");
            } else {
                println!("Dette er ikke en mappe");
            }
        }
        Err(_) => println!("Filstien finnes ikke"),
    }
}
```
Begge disse metodene er vanlig praksis i Rust programmering, men hvilken en du velger å bruke kan avhenge av hvilken type data du trenger for ditt spesifikke case.

## Se Også
- [Rust Dokumentasjon på Path](https://doc.rust-lang.org/std/path/)
- [Rust Dokumentasjon på metadata()](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Rust Dokumentasjon på exists()](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists) 

Dette bør gi deg en god start på å sjekke om mapper eksisterer i Rust.