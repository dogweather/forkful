---
title:                "Store bokstaver i en tekst"
html_title:           "Rust: Store bokstaver i en tekst"
simple_title:         "Store bokstaver i en tekst"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekstbehandling i Rust, har du kanskje lurt på hvordan du kan få en streng til å starte med stor bokstav. Enten det er for å skape et profesjonelt utseende på en nettside eller for å følge konvensjoner i koding, så er det viktig å kunne gjøre dette enkelt. I denne artikkelen vil vi se på hvordan du kan kapitalisere en streng i Rust.

## Hvordan gjør du det

Det finnes flere måter å kapitalisere en streng i Rust på, men den enkleste og mest effektive måten er å bruke metoden `to_uppercase()`. Dette er en innebygd metode som blir brukt på en `String`-type for å transformere alle små bokstaver til store bokstaver.

For eksempel, hvis vi har følgende kode:

```Rust
fn main() {
    let tekst = "hei på deg";
    let kapitalisert_tekst = tekst.to_uppercase();
    println!("{}", kapitalisert_tekst);
}
```

Så vil utskriften bli `HEI PÅ DEG`. Som du kan se, så blir alle små bokstaver omgjort til store bokstaver med denne metoden.

## Deep Dive

Hvis du ønsker å gå dypere inn i hvordan denne metoden fungerer, så kan vi se på dens signatur:

```Rust
fn to_uppercase(&self) -> Cow<str>
```

`Cow` står for "copy on write", og er en datastruktur som brukes for å effektivt håndtere strenger i Rust. Metoden tar i mot en referanse til en streng og returnerer en `Cow`-type. Siden `Cow` er en "smart" datastruktur, vil den effektivt håndtere kopiering av strengen hvis den er nødvendig. Dette gjør at metoden `to_uppercase()` er rask og effektiv.

I tillegg til å bruke `to_uppercase()`-metoden, kan du også bruke andre metoder som `to_ascii_uppercase()` og `to_lowercase()` for å endre bokstavstørrelsen på strenger i Rust.

## Se også

- [Rust sin dokumentasjon for `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Offisiell Rust-nettside](https://www.rust-lang.org/)

Nå vet du hvordan du kan enkelt kapitalisere en streng i Rust. Ved å bruke `to_uppercase()`-metoden, kan du spare tid og energi på å manuelt endre bokstavstørrelsen på strenger. Lykke til med din tekstbehandling i Rust!