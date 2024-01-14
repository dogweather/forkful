---
title:    "Rust: Sjekke om en mappe eksisterer"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Et viktig aspekt ved programmering er å sikre at koden din håndterer potensielle problemer som kan oppstå under kjøring. En av disse utfordringene er å sjekke om en mappe eksisterer før du prøver å utføre operasjoner på den. Dette er spesielt nyttig når du håndterer filer og mapper som brukeren selv kan ha skapt eller slettet.

## Hvordan

I Rust kan du sjekke om en mappe eksisterer ved å bruke std::fs biblioteket og dens funksjon `.metadata()` som returnerer metadata for en fil eller en mappe. Ved å bruke `is_dir()` metoden på denne metadatatypen, kan du få informasjon om hvorvidt den gitte filen er en mappe eller ikke. Hvis filen er en mappe, betyr det at mappen eksisterer, hvis ikke, betyr det at mappen ikke finnes.

For å gjøre dette enda enklere, kan du pakke dette inn i en funksjon som tar inn en streng som representerer mappen du ønsker å sjekke. Her er et eksempel på hvordan det kan se ut:

```Rust
use std::fs;

fn check_directory_exists(path: &str) -> bool {
    let metadata = fs::metadata(path);

    match metadata {
        Ok(metadata) => metadata.is_dir(),
        Err(_) => false,
    }
}

fn main() {
    let directory_exists = check_directory_exists("my_directory");
    println!("My directory exists: {}", directory_exists);
}
```

I dette eksempelet, vil funksjonen sjekke om mappen "my_directory" eksisterer og returnere en bool verdi avhengig av resultatet. Deretter vil denne verdien skrives ut i konsollen. Hvis mappen eksisterer, vil utgangen bli "true", hvis ikke, vil utgangen bli "false".

## Dypdykk

Når vi bruker `.metadata()` funksjonen, vil det returnere en `Result` type som enten vil inneholde metadataen for mappen eller en feil hvis mappen ikke finnes. Dette betyr at vi må bruke `match` for å håndtere begge tilfellene.

Det er også verdt å merke seg at denne funksjonen kan føre til at programmet ditt henger hvis mappen du prøver å sjekke inneholder en stor mengde filer. Dette skyldes at `.metadata()` funksjonen vil prøve å lese informasjon om alle filene i mappen, noe som kan være tidskrevende. Dette problemet kan løses ved å bruke `.exists()` funksjonen i stedet, som bare sjekker om mappen eksisterer uten å lese all metadata.

## Se også

- [Rust std::fs dokumentasjon](https://doc.rust-lang.org/std/fs/index.html)
- [Rust resultater og håndtering av feil](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Sjekke om en fil eksisterer i Rust](https://mattschulte.github.io/2020/02/23/check-if-file-exists-in-rust-html)