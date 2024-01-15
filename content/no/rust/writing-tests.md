---
title:                "Skriving av tester"
html_title:           "Rust: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av utviklingen av programvare. Det bidrar til å avdekke feil og sikre at koden fungerer som den skal, noe som resulterer i mer pålitelige og robuste applikasjoner.

## Hvordan

For å skrive tester i Rust, må du først importere test-rust biblioteket ved å legge til følgende linje øverst i filen din:

```Rust
use test_rust::assert_eq;
```

Deretter kan du bruke `assert_eq` funksjonen for å sammenligne verdier og sikre at de er like. Her er et eksempel på en enkel testfunksjon som sjekker om to tall er like:

```Rust
fn test_addition() {
    let num1 = 2;
    let num2 = 3;
    let result = num1 + num2;
    assert_eq(result, 5);
}
```

Når du kjører testene dine, vil du få følgende utgang:

```
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

Dette betyr at testen bestått og at koden fungerer som forventet. Hvis testen hadde feilet, ville du fått en feilmelding med informasjon om hva som gikk galt.

## Dypdykk

Tests i Rust er basert på enhetstesting, som fokuserer på å teste enkeltdeler av koden. Dette gjør at du kan isolere og teste mindre deler av koden din, noe som gjør det lettere å finne og rette feil.

For å skrive kvalitets tester, må du sørge for å teste både positive og negative scenarier. Det vil si å teste at koden fungerer som forventet når alt går rett, men også når noe går galt. Dette vil bidra til å sikre at koden din er robust og kan håndtere uventet input.

Det er også viktig å opprettholde en god dekningsgrad på testene dine. Det vil si at testene bør dekke så mye av koden din som mulig for å sikre at alle deler fungerer som de skal. Rust har et innebygd verktøy, `cargo`, som kan hjelpe deg med å måle testdekningsgraden din.

## Se også

For mer informasjon om testing i Rust, sjekk ut følgende ressurser:

- [The Rust Book](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust Coding Guidelines for Testing](https://doc.rust-lang.org/1.10.0/book/advanced-testing.html)
- [Offisiell Rust Dokumentasjon for Test](https://doc.rust-lang.org/test/)