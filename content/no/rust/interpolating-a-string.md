---
title:                "Interpolering av streng"
html_title:           "Rust: Interpolering av streng"
simple_title:         "Interpolering av streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å interpolere en streng er en teknikk som tillater programmerere å sette inn variabler eller uttrykk direkte inn i en string. Dette gjør det enklere og mer effektivt å bygge dynamiske meldinger eller utskrifter, spesielt når variabler eller uttrykk endres i løpet av programkjøringen.

## Hvordan:
```Rust
let navn = "Nina";
let alder = 25;
let beskjed = format!("Hei, mitt navn er {} og jeg er {} år gammel.", navn, alder);
println!("{}", beskjed);
```
Output: Hei, mitt navn er Nina og jeg er 25 år gammel.

## Dykk dypere:
Interpolering av strenger ble først introdusert i programmeringsspråket Perl i 1987, og har senere blitt adoptert av mange andre språk, inkludert Rust. Det finnes også alternative måter å interpolere strenger på, som for eksempel bruk av escape-sekvenser eller string concatenation. Implentasjonen av string interpolation i Rust er basert på format! makroen, som tillater at en string og variabler eller uttrykk kombineres på en enkel og elegant måte.

## Se også:
- https://doc.rust-lang.org/book/ch08-02-strings.html
- https://en.wikipedia.org/wiki/String_interpolation