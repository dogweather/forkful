---
title:                "Rust: Konvertering av en streng til små bokstaver"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være en nødvendig oppgave når du jobber med tekstbehandling eller søk i tekstbaserte data. Dette kan hjelpe deg med å gjøre søk mer presise og effektive, samtidig som det gir et jevnere utseende på teksten din.

## Hvordan

I Rust er det en enkel måte å konvertere en streng til små bokstaver ved hjelp av standardbibliotekets `to_lowercase()` funksjon. La oss se på et eksempel:

```Rust
let streng = "HELLO, VERDEN!";
let konvertert_streng = streng.to_lowercase();

println!("Original streng: {}", streng);
println!("Konvertert streng: {}", konvertert_streng);
```

Dette vil produsere følgende utgang:

```
Original streng: HELLO, VERDEN!
Konvertert streng: hello, verden!
```

Som du kan se, er den originale strengen blitt konvertert til små bokstaver. Dette er enkelt og effektivt når du trenger å gjøre en lignende operasjon på tekst.

## Dypdykk

Det er verdt å merke seg at Rusts `to_lowercase()` funksjon kun støtter ASCII-tegn, noe som betyr at spesialtegn og bokstaver fra andre språk kan bli konvertert til uventede verdier. I tillegg er denne funksjonen ikke språksensitiv, så den vil ikke konvertere bokstaver med spesielle regler i noen språk.

Imidlertid eksisterer det biblioteker i Rust som håndterer disse begrensningene, som for eksempel `unicode_normalization`, som kan konvertere tekst til små bokstaver på en mer omfattende og språksensitiv måte.

## Se også

- [Offisiell dokumentasjon for `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust Cookbook: Konverter tekst til små bokstaver](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#convert-text-to-lowercase)