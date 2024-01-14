---
title:                "Rust: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvorfor noen programmer krever at teksten du skriver inn er i små bokstaver? Kanskje du har sett en feilmelding som ber deg om å konvertere teksten til små bokstaver? I denne blogginnlegget skal vi se på hvorfor det er viktig å konvertere en tekst til små bokstaver, og hvordan du kan gjøre det enkelt ved hjelp av Rust-programmeringsspråket.

## Hvordan gjøre det
For å konvertere en teksten til små bokstaver i Rust, kan du bruke funksjonen "to_lowercase". La oss se på et eksempel på hvordan dette kan gjøres:

```rust
let tekst = "Hei alle sammen!";
let konvertert_tekst = tekst.to_lowercase();
println!("Konvertert tekst: {}", konvertert_tekst);
```

Dette vil gi følgende utskrift:

```rust
Konvertert tekst: hei alle sammen!
```

Som du kan se, ble alle bokstaver i teksten konvertert til små bokstaver. Dette kan være nyttig når du for eksempel ønsker å sammenligne to tekster, og ikke vil at forskjellige stavemåter skal påvirke resultatet.

## Dypdykk
Nå som vi har sett på hvordan du kan konvertere en tekst til små bokstaver i Rust, la oss se på noen andre nyttige egenskaper ved denne funksjonen. For det første er "to_lowercase"-funksjonen casesensitiv, noe som betyr at store bokstaver vil bli konvertert til små bokstaver, mens små bokstaver ikke vil bli påvirket.

Det er også verdt å merke seg at denne funksjonen fungerer for både engelsk og ikke-engelske språk, og vil behandle spesielle bokstaver som "æ", "ø" og "å" på riktig måte.

## Se også
- [Dokumentasjon for to_lowercase-funksjonen i Rust](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [En oversikt over Rust-programmeringsspråket](https://www.rust-lang.org/no)
- [En nybegynnerguide til Rust](https://stevedonovan.github.io/rust-gentle-intro/1-introduction.html)