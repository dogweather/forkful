---
title:    "Rust: Slette tegn som samsvarer med et mønster"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

I noen programmeringsspråk, som Rust, kan man noen ganger komme over en situasjon hvor man ønsker å slette visse tegn som matcher et bestemt mønster. Dette kan være for å rydde opp i data, fjerne unødvendig informasjon eller for andre formål. Å kunne slette slike tegn på en effektiv og korrekt måte er viktig for å opprettholde et ryddig og lesbart kodebase.

## Hvordan

For å kunne slette tegn som matcher et mønster i Rust, kan man bruke metoden “retain” på en mutabel streng. Denne metoden tar inn en closure som parameter og evaluerer hvert tegn i strengen mot mønsteret som er gitt. Tegnene som matcher vil bli beholdt, mens de som ikke matcher vil bli slettet.

```Rust
let mut tekst = "Hei, dette er en test!".to_string();
tekst.retain(|c| c != 'i'); // sletter alle "i" i teksten
println!("{}", tekst);
```
Output: "He, dette er en test!"

Her har teksten blitt endret ved at alle "i" er blitt slettet, og man sitter igjen med en renset versjon av teksten.

## Dypdykk

Det finnes flere måter å slette tegn som matcher et mønster i Rust, men metoden med å bruke "retain" er en enkel og effektiv løsning. Man kan også bruke regex-biblioteket for å finne og slette mønstre, men dette krever litt mer av koden. Det er viktig å huske på at slettingen av tegn kan påvirke andre deler av koden, så det er viktig å sjekke at alt fungerer som ønsket etter slettingen.

## Se også

- [Offisiell Rust dokumentasjon](https://www.rust-lang.org/)
- [Rust programmingspråkets nettside](https://www.rust-lang.org/)