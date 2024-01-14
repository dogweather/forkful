---
title:                "Rust: Konvertere en dato til en streng"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmeringsprosjekter krever at man må konvertere en dato til en streng (string) for å vise den på en lesbar måte for brukeren. Dette er spesielt viktig i applikasjoner som inneholder kalendere eller tidslinjer. I Rust, kan dette gjøres ved hjelp av standardbibliotekets `DateTime` og `format` moduler. I denne bloggposten skal vi utforske hvordan man kan konvertere en dato til en streng i Rust.

## Hvordan gjøre det

Først må vi importere `DateTime` og `format` modulene fra Rusts standardbibliotek:

```Rust
use std::time::DateTime;
use std::time::Instant;
use std::str::FromStr;
```

Deretter må vi definere en dato ved hjelp av `DateTime` sin `from_str` funksjon og angi den ønskede datoformaten ved hjelp av `format` sin `parse` funksjon:

```Rust
let date = DateTime::from_str("2021-05-31T15:30:00.00000000Z").unwrap();
let formatted_date = date.format("%d.%m.%Y kl. %H:%M").to_string();
```

Til slutt må vi konvertere datoen til en streng ved hjelp av `to_string` funksjonen og lagre den i en variabel.

## Dypdykk

Nå som vi har konvertert datoen til en streng, kan vi også formatere den på ulike måter. For eksempel kan vi endre rekkefølgen på dag, måned og år ved å bruke `%m.%d.%Y` istedenfor `%d.%m.%Y` i formatstrengen. Vi kan også inkludere månedsnavnet ved å bruke `%B` og årstallet uten forkortelse ved å bruke `%Y` istedenfor `%y`.

Det er også viktig å merke seg at `format` modulen støtter å formatere datoer på ulike språk. Dette kan gjøres ved å spesifisere språkkoden i formatstrengen, for eksempel `%d.%m.%Y` for norsk og `%m/%d/%Y` for engelsk.

## Se også

- [Official Rust documentation for DateTime](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Official Rust documentation for format module](https://doc.rust-lang.org/std/fmt/index.html)
- [Date and time formatting in Rust](https://www.educative.io/blog/rust-tutorial-datetime)