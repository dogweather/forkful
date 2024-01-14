---
title:    "Rust: Sjekke om en mappe eksisterer"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering i Rust, spesielt når du jobber med filsystemer. Dette kan hjelpe deg med å håndtere feil og sikre at programmet ditt fungerer som det skal.

## Hvordan

For å sjekke om en mappe eksisterer i Rust, kan du bruke funksjonen `Path::is_dir()` fra standardbiblioteket. Her er et eksempel på hvordan du kan bruke denne funksjonen i et program:

```Rust
use std::fs;

fn main() {
    let directory = "min_mappe";

    if fs::metadata(directory).is_ok() && fs::metadata(directory).unwrap().is_dir() {
        println!("Mappen {} eksisterer!", directory);
    } else {
        println!("Mappen {} eksisterer ikke.", directory);
    }
}
```

I dette eksempelet bruker vi først `fs::metadata()` for å hente metadata om mappen. Hvis dette er vellykket, kan vi bruke `is_dir()` for å sjekke om det er en mappe. Hvis det er tilfelle, vil vi få en beskjed om at mappen eksisterer.

## Dypdykk

Når vi bruker `fs::metadata()` for å sjekke om mappen eksisterer, må vi også håndtere eventuelle feil som kan oppstå. Dette kan gjøres ved hjelp av `Result`-typen og `unwrap()`-metoden.

En annen måte å sjekke om en mappe eksisterer på, er å bruke `Path::exists()` i stedet for `is_dir()`. Forskjellen er at `exists()` vil returnere `true` selv om det er en fil og ikke en mappe med det aktuelle navnet.

Det er også verdt å merke seg at disse funksjonene bare sjekker om en fil eksisterer og ikke om du har tilgang til den. Derfor er det fortsatt viktig å håndtere eventuelle feil som kan oppstå når du prøver å åpne en fil eller mappe.

## Se også

- [Dokumentasjon for stienes koplinger (Path links)](https://doc.rust-lang.org/std/path/index.html#links)
- [Rust std::fs dokumentasjon](https://doc.rust-lang.org/std/fs/index.html)
- [Hvordan håndtere feil og unntak i Rust](https://www.rust-lang.org/learn/errors)