---
title:                "Rust: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Regular expressions er et kraftig verktøy for strukturert tekstbehandling. Enten du leter etter bestemte ord eller mønster i store mengder tekst, kan regular expressions hjelpe deg med å finne og manipulere informasjonen du trenger.

## Slik gjør du det
For å bruke regular expressions i Rust, må du først importere "regex" biblioteket. Deretter kan du opprette et nytt Regex-objekt og bruke forskjellige metoder som "find" og "captures" for å finne og manipulere tekst. Her er et eksempel på hvordan du kan bruke regular expressions for å finne og erstatte et ord i en tekst:

```Rust
use regex::Regex;

// Opprett et nytt Regex-objekt med ønsket mønster
let re = Regex::new(r"\b(heisann)\b").unwrap();

// Definer teksten som skal sjekkes
let text = "Heisann! Hvordan går det?";

// Bruk metoden "replace_all" for å erstatte "heisann" med "hallo"
let result = re.replace_all(text, "hallo");

// Skriv ut det nye resultatet
println!("{}", result); // Hallo! Hvordan går det?
```

I dette eksempelet bruker vi mønsteret "\b(heisann)\b" for å finne ordet "heisann" kun når det står som et eget ord, og ikke som en del av et større ord. Dette er et enkelt eksempel, men med regular expressions kan du lage mer komplekse mønstre for å finne og manipulere tekst på ulike måter.

## Dypdykk
Regular expressions støtter også metakarakterer, som gir deg enda mer kraft til å finne spesifikk tekst. For eksempel kan du bruke "+" for å finne ett eller flere forekomster av et tegn eller gruppe av tegn, "*" for å finne null eller flere forekomster, og "?" for å finne null eller én forekomst. Dette kan være nyttig når du skal finne tekst som varierer i format eller lengde.

Det finnes også mange forskjellige metoder du kan bruke sammen med Regex-objekter, for eksempel "is_match" for å sjekke om et mønster finnes i en tekst, eller "captures_iter" for å iterere gjennom alle forekomster av et mønster. Det er også mulig å bruke flags for å utføre søk og erstatninger med forskjellige innstillinger, som å ignorere store og små bokstaver.

Hvis du ønsker å lære mer om regular expressions i Rust, kan du sjekke ut dokumentasjonen for "regex" biblioteket, eller se på forskjellige eksempler og artikler på nettet. Å forstå hvordan regular expressions fungerer og å bli fortrolig med bruken av dem, kan gjøre tekstbehandling i Rust mye mer effektivt og elegant.

## Se også
- [Rust "regex" bibliotek dokumentasjon](https://docs.rs/regex)
- [Offisiell Rust nettside](https://www.rust-lang.org/)
- [Tutorial: Regex in Rust, a simple tutorial](https://sjollis.com/2020/06/17/regex-in-rust-a-simple-tutorial/)