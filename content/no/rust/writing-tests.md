---
title:    "Rust: Skrive tester"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i programmering er viktig for å sikre at koden vår fungerer som den skal. Det hjelper oss med å fange feil og feil tidlig i utviklingsprosessen, slik at vi kan fikse dem før de blir et større problem. Det er også en god praksis for å sikre at koden vår er pålitelig og skalerbar.

## Slik gjør du det

For å skrive tester i Rust, må du først importere "test" biblioteket. Deretter kan du bruke makroen "assert_eq!" for å teste om to verdier er like. For eksempel:

```rust
#[test]
fn test_addition() {
    let result = 2 + 2;
    assert_eq!(result, 4);
}
```

Dette vil kjøre en test som vil passere hvis resultatet er lik 4, og feile hvis det ikke er det. Du kan også bruke "assert!" makroen for å teste om en betingelse er sant.

```rust
#[test]
fn test_even_number() {
    let number = 4;
    assert!(number % 2 == 0);
}
```

Denne testen vil passere fordi 4 er et partall, men hvis du endrer tallet til 3, vil testen feile.

## Dykk dypere

Det er mange flere ting du kan gjøre med Rust-tester, som å teste for panikk eller forventet feil, or å skrive tester for funksjoner med kompliserte returverdier. Du kan også skrive integrasjonstester som tester hele applikasjonen din for å sikre at alle deler fungerer riktig sammen.

Det er også viktig å bruke kodecoverapportering for tester i Rust. Dette hjelper deg med å identifisere områder av koden din som ikke er tilstrekkelig testet og kan bidra til å forbedre kvaliteten på koden din ytterligere.

## Se også

For mer informasjon om å skrive tester i Rust, sjekk ut disse nyttige ressursene:

- ["Rust Book" om testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [The "assert" module documentation](https://doc.rust-lang.org/std/assert/index.html)
- [Offisielle Rust dokumentasjon om kodedekning](https://doc.rust-lang.org/nightly/unstable-book/compiler-flags/code-coverage.html)