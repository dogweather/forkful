---
title:    "Rust: Konvertere en streng til lowercase"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Stringene er viktige datastrukturer i de fleste programmeringsspråk, inkludert Rust. Det er derfor nyttig å kunne konvertere strings til ulike formater for å kunne håndtere dem på en enklere og mer konsistent måte. Å konvertere en string til lower case gir en mer standardisert måte å håndtere tekst på, noe som kan være nyttig i flere programmerings- og databehandlingsoppgaver.

## Hvordan gjøre det

For å konvertere en string til lower case i Rust, kan du bruke funksjonen `to_lowercase()` fra standardbiblioteket til Rust. Her er et enkelt eksempel på hvordan dette kan gjøres:

```Rust
let string = "DETTE ER EN STRING";
let lower_case = string.to_lowercase();
```

I dette eksemplet blir den opprinnelige stringen "DETTE ER EN STRING" konvertert til "dette er en string" og lagret i en variabel som heter `lower_case`. For å gjøre dette må du også importere `String`-typen fra preludiet til Rust.

## Dypdykk

Det å konvertere en string til lower case kan virke som en enkel oppgave, men det er noen aspekter som kan være nyttige å være klar over. For det første vil konverteringen avhenge av konteksten til programmet ditt og hvilken språkinnstilling som brukes. Det er også viktig å huske på at noen bokstaver har forskjellige former i ulike språk, for eksempel "ß" som blir konvertert til "ss" i lower case. Derfor er det alltid viktig å teste og validere konverteringen for å sikre nøyaktighet.

En annen ting å huske på er at `to_lowercase()` fungerer på en klon av den opprinnelige stringen, og du må derfor håndtere returverdien fra funksjonen på en annen måte hvis du trenger å beholde den opprinnelige stringen uendret.

## Se også

- [Rust dokumentasjon for `String` og `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [En utfyllende forklaring på konvertering av bokstaver mellom ulike språk i Rust](https://users.rust-lang.org/t/strings-unicode-lang-hints-and-lowercase/25562)