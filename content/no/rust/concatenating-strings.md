---
title:    "Rust: Sammenstilling av strenger"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen eller "konkatenere" strenger er en vanlig og viktig oppgave i programmering. Dette betyr rett og slett å kombinere to eller flere strenger for å danne en lengre streng. Dette kan være nyttig for å lage dynamiske tekstbeskjeder, konstruere URL-er og mye mer.

## Slik gjør du det

I Rust kan du enkelt konkatenerere strenger ved å bruke operatoren "+" eller metoden ".push_str()". La oss se på noen eksempler:

- Konkatenering med "+":

```rust
let navn = "Ole";
let etternavn = "Nordmann";

let fulltNavn = navn + " " + etternavn;

println!("{}", fulltNavn); // Output: Ole Nordmann
```

- Konkatenering med ".push_str()":

```rust
let navn = "Kari";
let etternavn = "Nordmann";

let mut fulltNavn = String::from(navn);

fulltNavn.push_str(" ");
fulltNavn.push_str(etternavn);

println!("{}", fulltNavn); // Output: Kari Nordmann
```

I eksemplene over blir to strenger slått sammen for å danne en lengre streng, som deretter blir skrevet ut til konsollen. Det er viktig å merke seg at for å kunne endre en streng, må den deklareres som "mut" (mutabel) ved å bruke nøkkelordet "mut" foran variabelnavnet.

Det finnes også andre metoder for å konkatenerere strenger, som for eksempel ".format()".

## Dypdykk

Rust har en mer effektiv måte å konkatenerere strenger på enn å bruke operatoren "+". Dette kalles "string formatting" og kan gjøres ved hjelp av makroen "format!".

```rust
let farge = "rød";
let vekt = 5.3;

let resultat = format!("Denne boksen er {} og veier {} kg.", farge, vekt);

println!("{}", resultat); // Output: Denne boksen er rød og veier 5.3 kg.
```

Denne metoden tillater også spesifisering av variabeltype og annen formatiering av strengen. Du kan lese mer om dette i Rusts offisielle dokumentasjon.

## Se også

- [Rust strenger](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Offisiell Rust dokumentasjon om "string formatting"](https://doc.rust-lang.org/std/fmt/)