---
title:                "Fjerning av tegn som matcher et mønster"
html_title:           "Rust: Fjerning av tegn som matcher et mønster"
simple_title:         "Fjerning av tegn som matcher et mønster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle man engasjere seg i å slette tegn som matcher et mønster? Vel, det kan være nyttig når du jobber med tekstbehandling eller datafiltrering for å rydde opp og få en mer organisert og lesbar datastrøm.

## Hvordan

Før vi går inn i kodene, la oss først definere hva vi mener med "mønster". Et mønster er en serie tegn som vi ønsker å finne i en tekststreng. For å slette disse tegnene, kan vi bruke et konsept som kalles "regex" eller regulære uttrykk. 

For å bruke regex i Rust, må du først importere regex-biblioteket ved å legge til følgende linje øverst i koden din:

```Rust
use regex::Regex;
```

Deretter kan du definere mønsteret du vil slette ved hjelp av Regex-structen og Regex::new-metoden:

```Rust
let pattern = Regex::new(r"tegn"); // r angir en raw string
```

Nå har vi definert mønsteret "tegn". For å slette dette mønsteret fra en tekststreng, kan vi bruke Regex::replace-metoden:

```Rust
let text = pattern.replace_all("Dette er noen tegn som skal slettes.", "");
```

I dette tilfellet har vi satt erstatningsteksten til å være en tom streng, noe som betyr at alle forekomster av mønsteret "tegn" vil bli slettet fra teksten. 

Her er den komplette koden:

```Rust
use regex::Regex;

fn main() {
    let pattern = Regex::new(r"tegn").unwrap();
    let text = pattern.replace_all("Dette er noen tegn som skal slettes.", "");
    println!("{}", text) // Output: Dette er noen som skal slettes.
}
```

Som du kan se, har alle forekomster av "tegn" blitt slettet fra teksten.

## Dykk Dypere

Det er viktig å merke seg at regex-mønstre bruker et spesielt syntaks som kan være litt vanskelig å forstå i begynnelsen. Her er noen vanlige syntakser som kan være nyttige å kjenne:

- `.`: Tilsvarer et hvilket som helst tegn.
- `*`: Tilsvarer null eller flere forekomster av det forrige uttrykket.
- `+`: Tilsvarer én eller flere forekomster av det forrige uttrykket.
- `?`: Tilsvarer null eller én forekomst av det forrige uttrykket.
- `^`: Tilsvarer starten av teksten.
- `$`: Tilsvarer slutten av teksten.

Det finnes også mange flere syntakser og metoder som kan brukes i regex. Det er verdt å gjøre noen undersøkelser for å lære mer om dette kraftige verktøyet.

## Se Også

- [Rust regex bibliotek](https://docs.rs/regex/1.5.0/regex/)
- [Rust programmeringsspråk offisiell hjemmeside](https://www.rust-lang.org/no)
- [Regex cheat sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)