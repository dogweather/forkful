---
title:                "Rust: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
I denne bloggposten skal vi se nærmere på hvordan man kan sjekke om en mappe eksisterer i Rust-programmeringsspråket. Dette er en viktig ferdighet for å kunne håndtere filbehandling i et program, og kan være nyttig i mange ulike situasjoner.

## Slik gjør du det
For å sjekke om en mappe eksisterer i Rust, kan du bruke funksjonen ```std::path::Path::exists()```. Denne funksjonen tar inn en sti (path) som parameter og returnerer en boolean-verdi som indikerer om stien eksisterer eller ikke.

La oss se på et eksempel. Anta at vi har en mappe med navnet "Bilder" på skrivebordet vårt. Vi kan bruke følgende kode for å sjekke om denne mappen eksisterer:

```Rust
use std::path::Path;

if Path::new("C:\\Users\\brukernavn\\Desktop\\Bilder").exists() {
    println!("Bilder-mappen eksisterer!");
} else {
    println!("Bilder-mappen eksisterer ikke!");
}
```

Dette vil skrive ut "Bilder-mappen eksisterer!" hvis mappen finnes, og "Bilder-mappen eksisterer ikke!" hvis den ikke finnes.

I tillegg til å bruke ```std::path::Path::exists()```, kan du også bruke funksjonen ```std::fs::metadata()``` for å få mer informasjon om en mappe eller fil. Denne funksjonen returnerer en ```std::fs::Metadata```-struktur som inneholder blant annet informasjon om størrelse og endringsdato for den gitte stien.

## Dykk ned i detaljene
Når du bruker ```std::path::Path::exists()``` for å sjekke om en mappe eksisterer, må du være oppmerksom på at denne funksjonen også vil returnere ```true``` hvis stien peker på en fil. Hvis du kun ønsker å sjekke om en mappe (og ikke en fil) eksisterer, kan du bruke funksjonen ```std::fs::metadata()``` som nevnt tidligere, og sjekke om returverdien inneholder informasjon om en mappe.

Det er også viktig å merke seg at å sjekke om en mappe eksisterer ikke betyr at du har tilgang til å lese eller skrive til denne mappen. Dette vil fortsatt avhenge av filrettighetene og eventuelle restriksjoner på systemet ditt.

## Se også
- [Rust dokumentasjon for std::path::Path::exists()](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)
- [Rust dokumentasjon for std::fs::metadata()](https://doc.rust-lang.org/std/fs/fn.metadata.html)