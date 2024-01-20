---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:58:32.385413-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Kontrollere om en mappe eksisterer betyr å sjekke filsystemet for en spesifikk sti. Programmerere gjør dette for å unngå feil ved filhåndtering eller for å forsikre seg om at påvente operasjoner har en gyldig starttilstand.

## How to:
Rust har en innebygd funksjonalitet for å sjekke om en mappe eksisterer gjennom `std::path::Path` og `std::fs` bibliotekene. Her er et eksempel på bruk:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/en/tilfeldig/mappe");

    if path.exists() {
        println!("Mappen eksisterer.");
    } else {
        println!("Mappen eksisterer ikke.");
    }
}
```

Dette vil skrive ut:

```
Mappen eksisterer.
```

hvis mappen faktisk eksisterer på angitt sti, eller:

```
Mappen eksisterer ikke.
```

hvis den ikke gjør det.

## Deep Dive:
Lenge før Rust fantes, var slike operasjoner en del av de fleste programmeringsspråkene. I Rust er det trygt og robust på grunn av språkets fokus på sikkerhet og ytelse. Et alternativ til å bruke `exist()` er `metadata()`, som gir mer detaljert info, men for simpel eksistensjekk er `exists()` å anbefale.

Under panseret bruker `exists()` funksjonen systemkall for å interagere med filsystemet. På ulike operativsystemer kan dette bety forskjellige implementeringsdetaljer, men i Rust er disse abstrahert bort for å gi en konsekvent API.

## See Also:
- Offisiell Rust dokumentasjon for `Path`: https://doc.rust-lang.org/std/path/struct.Path.html
- `std::fs` modulen i Rust: https://doc.rust-lang.org/std/fs/
- Rust bok på handling av feil ved filoperasjoner: https://doc.rust-lang.org/book/ch09-00-error-handling.html