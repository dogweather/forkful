---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Rust: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig når du ønsker å sammenligne tekster uavhengig av store eller små bokstaver. Dette kan være spesielt nyttig i tilfeller der brukeren kan skrive inn tekst på forskjellige måter, men informasjonen fortsatt skal anses som lik.

## Slik gjør du det

Det er enkelt å konvertere en streng til små bokstaver i Rust ved hjelp av `to_lowercase()` funksjonen. Denne funksjonen tar imot en String eller en &str som argument og returnerer en ny String med alle bokstavene omgjort til små bokstaver.

```Rust
// Eksempel 1: Konverterer en String til små bokstaver:
let tekst = String::from("Hallo, Verden!");
let konvertert_tekst = tekst.to_lowercase();
println!("{}", konvertert_tekst); 
// Output: hallo, verden!
```
```Rust
// Eksempel 2: Konverterer en &str til små bokstaver:
let tekst = "Hallo, verden!";
let konvertert_tekst = tekst.to_lowercase();
println!("{}", konvertert_tekst);
// Output: hallo, verden!
```

## Dypdykk

Når man konverterer en streng til små bokstaver, blir hver enkelt bokstav transformert ved hjelp av Unicode standarden. Dette betyr at det ikke bare er de vanlige a til å bokstavene som blir omgjort, men også alle andre bokstaver i andre språk eller symboler. Dette gjør Rust til et flerspråklig venlig programmeringsspråk.

## Se også

- [`to_uppercase()` funksjonen](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Hvordan manipulere tekster i Rust](https://www.rust-lang.org/learn/strings)