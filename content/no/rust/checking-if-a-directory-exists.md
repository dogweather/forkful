---
title:                "Rust: Å sjekke om en mappe eksisterer"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmer trenger å sjekke om en bestemt mappe eksisterer før de kan gjøre en handling. Dette kan være for å sikre at filer ikke blir overskrevet, eller for å lage en ny mappe hvis den ikke finnes. I denne bloggposten vil jeg forklare hvordan man kan sjekke om en mappe eksisterer ved hjelp av Rust programmeringsspråket.

## Hvordan 
For å sjekke om en mappe eksisterer i Rust, kan vi bruke funksjonen ```std::fs::metadata```. Denne funksjonen returnerer en ```std::fs::Metadata``` struct som inneholder informasjon om en gitt fil eller mappe. Vi kan deretter bruke metoden ```std::fs::Metadata::is_dir()``` for å sjekke om det er en mappe.

```Rust
use std::fs;

let file_path = "/din/mappe/sti";

if let Ok(metadata) = fs::metadata(file_path) {
    if metadata.is_dir() {
        println!("Mappen eksisterer!");
    } else {
        println!("Mappen eksisterer ikke.");
    }
} else {
    println!("Kunne ikke finne metadata for gitt sti.");
}
```

Output:

```
Mappen eksisterer!
```

## Deep Dive
Hvis vi vil gå dypere inn i sjekkingen av en mappe, kan vi også bruke funksjonen ```std::path::Path::exists()```. Denne funksjonen sjekker direkte om en fil eller mappe eksisterer og returnerer et ```bool``` resultat.

```Rust
use std::path::Path;

let file_path = "/din/mappe/sti";

if Path::new(file_path).exists() {
    println!("Mappen eksisterer!");
} else {
    println!("Mappen eksisterer ikke.");
}
```

Output:

```
Mappen eksisterer!
```

Et annet alternativ er å bruke funksjonen ```std::fs::read_dir```, som returnerer en ```std::fs::ReadDir``` struct. Denne structen inneholder en iterator over filer og mapper i en gitt mappe. Hvis den gis en korrekt mappesti, vil iterator objektet inneholde minst én fil eller mappe. Hvis mappestien ikke eksisterer, vil iterator objektet være tomt.

```Rust
use std::fs;

let file_path = "/din/mappe/sti";

if let Ok(entries) = fs::read_dir(file_path) {
    if entries.count() > 0 {
        println!("Mappen eksisterer!");
    } else {
        println!("Mappen eksisterer ikke.");
    }
} else {
    println!("Kunne ikke åpne gitt sti.");
}
```

Output:

```
Mappen eksisterer!
```

## Se også
- [Rust dokumentasjon for filsystem operasjoner](https://doc.rust-lang.org/std/fs/index.html)
- [Enkel guide til å sjekke om en fil eksisterer i Rust](https://www.linode.com/docs/development/rust/how-to-check-if-file-or-directory-exists-in-rust/)
- [Offisiell Rust Discord-server](https://discord.gg/rust-lang)