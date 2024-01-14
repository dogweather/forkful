---
title:                "Rust: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

#Hvorfor

Å lese fra en tekstfil er en vanlig oppgave for mange programmer, spesielt når man jobber med data. I denne bloggposten skal vi utforske hvordan vi kan lese en tekstfil ved hjelp av programmeringsspråket Rust.

#Slik gjør du det

Først må vi importere biblioteket "std::fs" for å få tilgang til filsystemfunksjoner. Deretter kan vi bruke funksjonen "std::fs::File::open()" for å åpne en tekstfil. Hvis filen ikke finnes, vil dette føre til en feil som vi må håndtere.

```Rust
use std::fs;

let file = match fs::File::open("min_tekstfil.txt") {
    Ok(f) => f,
    Err(e) => panic!("Kunne ikke åpne filen: {}", e),
};
```

Nå kan vi lese innholdet i filen ved å bruke funksjonen "std::io::Read::read_to_string()" og lagre det i en variabel. Denne funksjonen tar inn en referanse til filen vår og en mutabel streng som vi kan lagre innholdet i.

```Rust
use std::io::Read;

let mut tekst = String::new();
match file.read_to_string(&mut tekst) {
    Ok(_) => (),
    Err(e) => panic!("Kunne ikke lese filen: {}", e),
}
```

Til slutt kan vi skrive ut innholdet i filen ved å bruke "println!" -makroen.

```Rust
println!("Innholdet i tekstfilen er:\n{}", tekst);
```

Dette vil gi følgende utskrift:

```
Innholdet i tekstfilen er:
Hei! Dette er en testfil.
```

#Dykk dypere

På samme måte som å lese fra en fil, kan vi også skrive til en fil ved hjelp av funksjonen "std::fs::File::create()". Vi kan også bruke strukturen "std::path::Path" for å håndtere filstier på en mer robust måte.

#Se også

- Rust dokumentasjon for filbehandling: https://doc.rust-lang.org/std/fs/
- En tutorial om hvordan å lese og skrive filer i Rust: https://www.tutorialspoint.com/rust/rust_file_io.htm