---
title:                "Rust: Skriving av tester"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å skrive stabil og pålitelig Rust-kode. Det hjelper deg med å oppdage feil og buggs tidlig i utviklingsprosessen, slik at du kan fikse dem før de forårsaker større problemer. Det gir også en god oversikt over koden din og gjør det enklere å forstå den.

## Hvordan

Det finnes flere forskjellige testrammeverk i Rust, men det mest populære er `cargo test` som er inkludert i Rust-verktøykassen.


For å skrive tester, må du lage et nytt bibliotek (eller mappe) i prosjektet ditt med navnet `tests`. Deretter kan du lage en ny fil med `.rs`-utvidelse inne i denne mappen og begynne å skrive testene dine.

La oss si at du har en funksjon `add` som tar to tall som argumenter og returnerer summen av dem. For å teste denne funksjonen, kan du skrive følgende kode i testfilen din:

```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[test]
fn test_add() {
    assert_eq!(add(2, 2), 4);
    assert_eq!(add(5, 10), 15);
}
```

I dette tilfellet bruker vi `assert_eq!`-makroen som sammenligner forventet og faktisk resultat. Hvis de er like, passerer testen, ellers feiler den.

Etter å ha skrevet testene dine, kan du kjøre dem ved å kjøre kommandoen `cargo test` i terminalen. Hvis alle testene dine passerer, vil du se en grønn melding om at testene er bestått. Hvis det oppstår en feil, vil testen mislykkes og du vil få en rød melding.

## Dypdykk

I tillegg til å bruke `assert_eq!`, er det også andre nyttige makroer og funksjoner som du kan bruke når du skriver tester. For eksempel kan du bruke `assert_ne!` for å teste at to verdier ikke er like, eller `assert!` for å teste vilkårlige påstander.

Det er også verdt å nevne at `cargo test` har flere alternativer som du kan bruke for å begrense eller filtrere testene dine. For å se alle tilgjengelige alternativer, kan du kjøre kommandoen `cargo test --help`.

## Se også

- [The Rust Programming Language - Kapittel 6: Testing your Code](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Introduction to Rust Testing](https://danielkeep.github.io/tlborm/book/mk-1-ch01.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/rust-by-example/testing.html)