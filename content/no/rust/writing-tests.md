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

## Hva & Hvorfor?

Å skrive tester er en viktig del av programmering. Det er en måte å sikre at koden vi skriver fungerer som forventet og unngå feil og bugs. Ved å skrive tester kan vi også få en bedre forståelse av koden vår og den gir mulighet for enklere debugging hvis noe går galt.

## Hvordan:

Testing i Rust er enkelt og intuitivt. Nedenfor vises et enkelt eksempel på hvordan man kan skrive en test for en funksjon som multipliserer to tall:

```Rust
fn multiply(x: i32, y: i32) -> i32 {
    x * y
}

#[test]
fn test_multiply() {
    assert_eq!(multiply(2, 3), 6);
}
```

I dette eksempelet bruker vi funksjonen `assert_eq!` for å sjekke om resultatet av multiplikasjonen er lik forventet verdi. Hvis dette ikke er tilfelle, vil testen feile og gi en feilmelding som hjelper oss med å finne og løse feilen.

## Dypdykk:

Å skrive tester har blitt en vanlig praksis innen programmering. Det sikrer at koden vår er pålitelig og fungerer som forventet. Alternativet til å skrive tester, er å manuelt teste koden vår hver gang vi gjør endringer, noe som tar mye tid og kan føre til menneskelige feil. I Rust, er tester en integrert del av språket og kompilatoren vår.

For å skrive effektive tester, er det viktig å forstå konseptet med enhetstesting og å kjenne til forskjellige typer tester som kan implementeres.

## Se Også:

- [Rust Testing Library](https://doc.rust-lang.org/book/ch11-00-testing.html) - Offisiell dokumentasjon for testing i Rust.
- [Unit Testing in Rust](https://medium.com/@jamesesutton/unit-testing-in-rust-549c78f36ada) - En guide til enhetstesting i Rust.
- [Rust Programming Language](https://www.rust-lang.org/) - Hjemmesiden til Rust-språket med ressurser og informasjon.