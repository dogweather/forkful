---
title:                "Ekstrahering av understrenger"
html_title:           "Rust: Ekstrahering av understrenger"
simple_title:         "Ekstrahering av understrenger"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substrings er en viktig del av programmering, spesielt når vi arbeider med tekstdokumenter og data. Dette gjør det mulig å hente ut spesifikke deler av en tekststreng, for eksempel en enkelt bokstav eller et ord, og bruke det til ulike formål i koden vår.

## Hvordan

Kodesnutter i Rust brukes til å trekke ut substrings ved hjelp av det innebygde biblioteket "String". La oss se på et eksempel på hvordan dette kan gjøres:

```Rust
let tekst = "Dette er en eksempeltekst";

let substring_1 = &tekst[0..5]; // Trekker ut de første 5 tegnene fra teksten
println!("{}", substring_1); // Output: Dette

let substring_2 = &tekst[12..19]; // Trekker ut tegnene mellom indeks 12 og 19 fra teksten
println!("{}", substring_2); // Output: eksempel

let substring_3 = &tekst[20..]; // Trekker ut alle tegn fra index 20 og utover
println!("{}", substring_3); // Output: tekst

```

Vi kan også bruke metoden "split()" for å dele opp teksten basert på en gitt separator, som for eksempel mellomrom eller komma.

```Rust
let tekst = "Dette er en, annen tekst";

let substring_1 = tekst.split("er"); // Deler opp teksten ved hvert forekomst av "er"
for substr in substring_1 {
    println!("{}", substr); // Output: D ett , , n , , n tekst
}

let substring_2 = tekst.split(", "); // Deler opp teksten ved hvert forekomst av et komma etterfulgt av et mellomrom
for substr in substring_2 {
    println!("{}", substr); // Output: Dette er en , annen tekst
}
```

## Dypdykk

Når det gjelder substrings i Rust, er det viktig å merke seg at det å trekke ut en del av en tekststreng resulterer i en ny tekststreng som er en referanse til den opprinnelige tekststrengen. Dette kan føre til uforutsette endringer i koden og derfor bør man være forsiktig med å manipulere substrings for mye.

Man kan også bruke "chars()" metoden for å få en iterator over enkelttegnene i en tekststreng og dermed gjøre operasjoner på hvert enkelt tegn.

## Se også

For mer informasjon og eksempler på hvordan man kan arbeide med substrings i Rust, kan du sjekke ut følgende ressurser:

- [Offisiell Rust dokumentasjon](https://doc.rust-lang.org/std/str/)
- [Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust Cookbook: Working with Strings](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html)