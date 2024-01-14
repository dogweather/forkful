---
title:    "Rust: Å lese en tekstfil"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig del av programmering, uansett språk. Det kan være nødvendig for å lese data fra en ekstern kilde, behandle informasjon eller generelt sett manipulere filer. I denne bloggposten vil vi se på hvordan man kan lese en tekstfil i programmeringsspråket Rust.

## Hvordan

Det første trinnet i å lese en tekstfil er å åpne den for lesing. Dette gjøres ved hjelp av standardbiblioteket i Rust, `std::fs`. Vi kan bruke funksjonen `File::open` for å åpne en fil og lagre den i en variabel. Dette vil se slik ut:

```rust
let fil = std::fs::File::open("tekstfil.txt").expect("Kunne ikke åpne filen.");
```

Deretter kan vi lese filen ved å bruke funksjonen `read_to_string`, som tar inn variabelen vår og returnerer en `String` med filens innhold. Dette kan se slik ut:

```rust
let innhold = std::fs::read_to_string(fil).expect("Kunne ikke lese filen.");
```

Etter å ha lest filen, er det viktig å huske å lukke den. Dette gjøres ved å kalle `close`-metoden på filvariabelen vår:

```rust
fil.close();
```

Etter å ha lest og behandlet filens innhold, kan vi gjøre videre operasjoner på dataene som trengs.

For å lese en tekstfil linje for linje, kan vi bruke en `BufReader` og en iterator. Dette sikrer at filen lukkes automatisk etter lesing:

```rust
let fil = std::fs::File::open("tekstfil.txt").expect("Kunne ikke åpne filen.");
let leser = std::io::BufReader::new(fil);
for linje in leser.lines() {
    let linje = linje.expect("Kunne ikke lese linjen.");
    println!("{}", linje);
}
```

## Dypdykk

Når vi åpner en tekstfil for lesing, bruker vi standard encodingen for systemet vårt. Dette kan være utf-8 eller en annen type encoding, som kan føre til problemer hvis filen har en annen encoding enn det vi forventer. I slike tilfeller kan vi spesifisere ønsket encoding når vi leser filen:

```rust
let fil = std::fs::File::open("tekstfil.txt").expect("Kunne ikke åpne filen.");
let innhold = std::fs::read_to_string(fil).expect("Kunne ikke lese filen.");
let utf8_innhold = std::str::from_utf8(innhold.as_bytes()).expect("Kunne ikke konvertere til utf-8.");
```

Det er også viktig å håndtere eventuelle feil som kan oppstå under lesing av filen, som for eksempel hvis filen ikke eksisterer eller hvis det oppstår en feil under lesing. Ved å bruke `expect`-metoden sikrer vi at programmet vårt håndterer disse feilene og gi en forklarende melding dersom en feil skulle oppstå.

## Se også

- [Rust Dokumentasjon: File](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Dokumentasjon: BufReader](https://doc.rust-lang.org/std/io/struct.BufReader.html)
- [Rust Dokumentasjon: read_to_string](https://doc.rust-lang.org/std/fs/fn.read_to_string.html)
- [Rust Dokumentasjon: lines](https://doc.rust-lang.org/std/io/struct.Lines.html)
- [Rust Dokumentasjon: from_utf8](https://doc.rust-lang.org/std/str/fn.from_utf8.html)