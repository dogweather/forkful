---
title:    "Rust: Lese en tekstfil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor lese en tekstfil?

Hvis du er en Rust-programmerer som jobber med å håndtere data, er det sannsynlig at du vil måtte lese data fra en tekstfil på et eller annet tidspunkt. Tekstfiler er en vanlig måte å lagre data på, og de kan inneholde alt fra tekst og tall til komplekse strukturer. Derfor er det viktig å vite hvordan man leser og behandler disse filene i Rust. I denne bloggposten skal vi utforske hvordan man kan håndtere tekstfiler i Rust.

## Slik leser du en tekstfil

For å lese en tekstfil i Rust, bruker vi File-modulen fra standardbiblioteket. Vi kan åpne en eksisterende fil ved å bruke `File::open`-funksjonen og gi den filbanen som argument. La oss se på et eksempel:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut fil = File::open("data.txt").expect("Kunne ikke åpne filen");
    let mut innhold = String::new();
    fil.read_to_string(&mut innhold).expect("Kunne ikke lese filen");
    println!("{}", innhold);
}
```

I dette eksempelet åpner vi filen "data.txt" og leser innholdet inn i en String-variabel. Vi bruker `expect`-metoden for å håndtere eventuelle feil som kan oppstå ved å åpne og lese filen. Deretter skriver vi ut innholdet til konsollen. Det er viktig å merke seg at vi også må importere `std::io::prelude` for å kunne bruke `read_to_string`-metoden.

## En dypere dykk

Nå som vi har sett på et enkelt eksempel på å lese en tekstfil, la oss utforske litt mer avanserte funksjoner. For eksempel kan vi lese og prosessere hver linje i en tekstfil ved å bruke `BufReader`-modulen. Denne modulen lar oss lese en tekstfil linje for linje og håndtere dataene i hver linje separat. La oss se på et eksempel på hvordan dette kan gjøres:

```Rust
use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let fil = File::open("data.txt").expect("Kunne ikke åpne filen");
    let leser = BufReader::new(fil);
    for linje in leser.lines() {
        let l = linje.expect("Kunne ikke lese linjen");
        println!("{}", l);
    }
}
```

I dette eksempelet bruker vi en for-løkke til å lese hver linje i filen og skrive den ut til konsollen. Vi bruker `expect`-metoden for å håndtere eventuelle feil som kan oppstå. Ved å bruke `BufReader` får vi tilgang til `lines()`-metoden, som returnerer en iterator over hver linje i filen.

## Se også

For mer informasjon om hvordan man kan håndtere tekstfiler i Rust, kan du sjekke ut disse ressursene:

- [Offisiell Rust dokumentasjon for File-modulen](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust By Example - Filbehandling](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file/open.html)
- [Rust Cookbook - Lesing og skriving av filer](https://rust-lang-nursery.github.io/rust-cookbook/file/reading-writing.html)

Lykke til med å håndtere tekstfiler i Rust!