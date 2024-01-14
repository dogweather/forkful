---
title:                "Rust: Å lese en tekstfil"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer eller bare nysgjerrig på Rust-programmeringsspråket, kan du lure på hvorfor du skal lese en tekstfil. Å lese en tekstfil er en vanlig oppgave i både personlige og profesjonelle programmer. Ved å lære hvordan du leser en tekstfil i Rust, kan du utvide dine programmeringsferdigheter og også lære om hvordan Rust håndterer data.

## Hvordan lese en tekstfil i Rust

For å lese en tekstfil i Rust, må du først opprette en ny fil og lagre den med en `.rs` filtype. Deretter kan du bruke `std::fs::File` biblioteket for å åpne filen og lagre den i en variabel:

```Rust
let fil = std::fs::File::open("path/to/file.txt").expect("Kunne ikke åpne filen!");
```

En gang åpnet, kan du bruke `std::io::BufReader` for å lese filen linje for linje:

```Rust
let fil = std::fs::File::open("path/to/file.txt").expect("Kunne ikke åpne filen!");
let leser = std::io::BufReader::new(fil);
```

Nå kan du bruke en `for`-løkke for å iterere over hver linje og skrive den ut til konsollen:

```Rust
for linje in leser.lines() {
    println!("{}", linje.unwrap());
}
```

Dette vil skrive ut hver linje i filen til konsollen. Du kan også bruke ulike biblioteker for å behandle tekstfilen på forskjellige måter, for eksempel å finne bestemte ord eller analyse av data.

## Dykk dypere

Å lese en tekstfil kan virke som en enkel oppgave, men det er faktisk mye som skjer under overflaten. For eksempel, hva skjer hvis filen ikke finnes eller hvis det oppstår en feil under lesingen? I Rust, er det viktig å håndtere disse potensielle feilene ved å bruke `Result`-typen og de innebygde `match`-uttrykkene.

Det er også verdt å merke seg at Rust bruker UTF-8 som standard for tekstfiler, så hvis du har en tekstfil med et annet tegnsett, må du bruke spesielle funksjoner for å håndtere dette.

## Se også

Har du lyst til å utvide kunnskapen din om Rust-programmering? Sjekk ut disse ressursene:

- Offisiell Rust dokumentasjon: https://www.rust-lang.org/no/learn
- Rust tutorials og kurs: https://www.rust-lang.org/no/learn
- Rust fellesskap: https://www.rust-lang.org/no/community

Lykke til med din Rust-programmeringsreise! Husk at å lese en tekstfil bare er en liten del av alt som Rust har å tilby.