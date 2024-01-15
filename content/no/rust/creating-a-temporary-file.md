---
title:                "Oppretting av en midlertidig fil"
html_title:           "Rust: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne artikkelen skal vi se på hvordan man kan opprette midlertidige filer ved hjelp av Rust-programmeringsspråket. Å opprette midlertidige filer kan være nyttig i situasjoner der man trenger å lagre data midlertidig eller til å teste ut kode uten å måtte skrive data til en permanent fil.

## Hvordan

For å opprette en midlertidig fil i Rust, kan vi bruke standardbiblioteket `std::fs::File` og dens `tempfile()`-funksjon. Vi kan bruke `tempfile()`-funksjonen for å opprette en midlertidig fil i et midlertidig katalog eller i standard midlertidig-katalogen for systemet.

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Opprette en midlertidig fil i standard midlertidig-katalogen
    let mut temp_file = File::tempfile().unwrap();
    temp_file.write_all(b"Dette er midlertidige data").unwrap();

    // Opprette en midlertidig fil i et midlertidig katalog
    let mut temp_dir = temp_dir!();
    temp_dir.push("midlertidig.txt");
    let mut temp_file_in_dir = File::create(&temp_dir).unwrap();
    temp_file_in_dir.write_all(b"Dette er også midlertidige data").unwrap();
}
```

Når vi kjører koden over, vil det bli opprettet to midlertidige filer i henholdsvis standard midlertidig-katalog og et midlertidig katalog på systemet. Vi skriver deretter noen eksempeldata til filene ved hjelp av `write_all()`-metoden. Det er viktig å merke seg at begge disse filene vil bli slettet automatisk når programmet avsluttes.

## Dypdykk

Når vi oppretter en midlertidig fil, kan vi også spesifisere noen attributter for filen. For eksempel kan vi endre filnavnet som blir generert av `tempfile()`-funksjonen. Dette kan være nyttig hvis man ønsker å gi en mer meningsfull filnavn til den midlertidige filen.

```Rust
// Spesifiser filnavn i `tempfile()`-funksjonen
let mut temp_file = File::tempfile_in("min-midlertidige-katalog").unwrap();
```

Man kan også spesifisere filtypen for den midldertidige filen, for eksempel kan man opprette en `.txt`-fil hvis man ønsker å teste en funksjon som tar inn en tekstfil som argument.

```Rust
// Spesifiser filtype
let mut temp_file = File::tempfile_with_suffix(".txt").unwrap();
```

Videre kan man spesifisere filmoduser som lesetilgang, skrivetilgang og tilgang for å kunne kjøre filen som et program. Dette kan være nyttig hvis man ønsker å teste forskjellige scenarier for en fil i et program.

```Rust
// Spesifiser filmoduser
let mut temp_file = File::tempfile_with_permissions(0o755).unwrap();
```

## Se også

- [Dokumentasjon for `std::fs::File` og `tempfile()`-funksjonen](https://doc.rust-lang.org/std/fs/struct.File.html#method.tempfile)
- [Eksempler på hvordan man bruker midlertidige filer i Rust](https://github.com/utfpr/mobile-test-rust/tree/master/src)
- [Mer om Rust-programmeringsspråket](https://www.rust-lang.org/no)