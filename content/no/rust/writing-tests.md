---
title:    "Rust: Skriving av tester"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i Rust kan virke som en ekstra byrde for mange utviklere, men det er en viktig del av utviklingsprosessen. Tester sikrer at koden vår fungerer som den skal, og gir oss tryggheten til å gjøre endringer i koden uten å bekymre oss for å ødelegge funksjonaliteten.

## Hvordan

For å skrive tester i Rust, må vi først importere modulen `test` i vår testfil. Deretter kan vi definere våre tester ved å bruke annoteringen `#[test]`.

Et eksempel på en testfunksjon i Rust ville sett slik ut:

```Rust
#[test]
fn add_two_numbers() {
    let result = add(2, 3);
    assert_eq!(result, 5);
}
```

Her har vi definert en testfunksjon kalt `add_two_numbers` som tester om funksjonen `add` gir riktig resultat når vi legger sammen 2 og 3. For å gjøre denne testen, bruker vi funksjonen `assert_eq!` som sammenligner det faktiske resultatet med det forventede. Hvis disse ikke er like, vil testen feile.

Vi kan også bruke flere assert-funksjoner for å teste forskjellige scenarier og sikre at vår kode fungerer som den skal.

## Dypdykk

Når vi skriver tester i Rust, er det viktig å huske på at disse testene også fungerer som dokumentasjon for koden. Derfor er det viktig å skrive testene våre på en forståelig og ryddig måte, slik at de kan leses som en informasjonskilde for andre utviklere.

Vi bør også prøve å teste så mange potensielle bugscenarier som mulig. Dette vil gi oss mer tillit til koden vår og sikre at den fungerer som forventet under alle omstendigheter.

Rust har også et innebygd verktøy kalt "cargo test" som lar oss kjøre alle testene våre automatisk. Dette gjør det enklere for oss å kontinuerlig teste koden vår mens vi utvikler, og sikrer at vi umiddelbart får beskjed om eventuelle feil.

## Se også

- [Rust dokumentasjon for testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust offisiell guide for enhetstesting](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
- [Enheten Testing i Rust med assert!(false)](https://dev.to/schell/enheten-testing-i-rust-med-assert-false-15d)