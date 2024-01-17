---
title:                "Søke og erstatte tekst"
html_title:           "Rust: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#Rust: Søk og erstatt tekst

## Hva og hvorfor?
Å søke og erstatte tekst betyr å finne en bestemt streng av tegn i en tekst og bytte den ut med en annen streng. Dette er en vanlig oppgave for programmerere når de ønsker å endre eller forbedre en tekstbasert kode. Ved å bruke dette verktøyet kan man effektivt gjøre endringer på en stor mengde tekst uten å måtte gjøre det manuelt.

## Slik gjør du det:
Søk og erstatt funksjonalitet finnes innebygd i Rust via standardbiblioteket. Her er et enkelt eksempel på hvordan man kan bruke den med ```str::replace()``` funksjonen:

```Rust
fn main() {
    let mut navn = "Per";
    println!("{}", navn); // Utskrift: "Per"

    navn = str::replace(navn, "Per", "Ole");
    println!("{}", navn); // Utskrift: "Ole"
}
```

## Dypdykk
Søk og erstatt tekst er en vanlig funksjon i mange programmeringsspråk og har vært en del av standardverktøyene i mange år. Alternativer til å bruke innebygde funksjoner inkluderer å bruke regexbiblioteker eller å bruke et tekstbehandlingsprogram for å utføre oppgaven manuelt.

## Se også:
- [Offisiell dokumentasjon for "str::replace()" funksjonen](https://doc.rust-lang.org/std/primitive.str.html#method.replace)
- [Rust regexbibliotek](https://docs.rs/regex/1.4.2/regex/)