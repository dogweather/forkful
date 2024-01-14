---
title:    "Gleam: Skriver tester"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i programmering kan virke som en kjedelig og unødvendig oppgave, men det kan faktisk være veldig nyttig. Tester hjelper deg med å sikre at koden din fungerer som det skal, og det kan også spare deg for mye frustrasjon og tid på lang sikt.

## Hvordan

Å skrive tester i Gleam er enkelt og kan gjøres ved hjelp av Gleam's integrerte test bibliotek. Først må du importere test biblioteket ved hjelp av `import gleam/test` og definere en test suite ved hjelp av `test.suite`.

Deretter kan du definere en test ved hjelp av `test.run` funksjonen, som tar inn en beskrivelse og en funksjon for testen. Innenfor funksjonen kan du bruke Gleam's `assert` funksjon for å teste om et utsagn er sant eller ikke. Her er et eksempel på en test som sjekker om 2+2 er lik 4:

```
Gleam run
import gleam/test
import assert

test.suite("Matematiske tester", [
  test.run("2+2 er lik 4", fn() {
    assert.equal(2 + 2, 4)
  })
])
```

Når du kjører denne testen, vil outputen være som følger:

```
Test suite: Matematiske tester
  ✔ 2+2 er lik 4
```

Du kan også bruke `assert` funksjonen til å teste for andre ting, som for eksempel å sjekke om en liste er tom eller ikke.

## Deep Dive

Det er mange andre måter å skrive tester på i Gleam, som for eksempel å bruke `test.Group` for å organisere testene dine i ulike grupper. Du kan også bruke Gleam's `lazy` funksjon for å kun kjøre en test hvis en annen test har feilet.

Det er også viktig å huske på at å skrive riktig og grundige tester vil hjelpe deg med å identifisere eventuelle feil i koden din, og gjøre det lettere å fikse dem før de blir et større problem.

## Se også

- [Gleam test dokumentasjon](https://gleam.run/book/tour/testing.html)
- [Hvorfor testing er viktig i programmering](https://medium.com/better-programming/why-testing-matters-aa0a29790c07)
- [Eksempler på gode tester i Gleam](https://github.com/gleam-lang/gleam/tree/master/core/test)