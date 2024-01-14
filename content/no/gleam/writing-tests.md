---
title:                "Gleam: Skrive tester"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av enhver god programmeringspraksis. Testene dine er din første forsvarslinje mot feil og sikrer at koden din fungerer som den skal. Å engasjere seg i å skrive tester vil også kunne forbedre din kodestil og struktur, noe som kan være til stor hjelp når du jobber med større og mer komplekse prosjekter.

## Hvordan

For å skrive tester i Gleam, kan du bruke modulen `gleam/test`. Først må du importere modulen slik at du kan få tilgang til testfunksjonene. Deretter kan du definere testene dine ved hjelp av `fn test/0`-funksjonen. Her er et enkelt eksempel på hvordan det kan se ut:

```
  # Example.gleam
  import gleam/test
  import Example
  
  fn test/0 {
    assert.equal("Hello World", Example.greeting())
  }
```

I dette eksempelet importerer vi `Example`-modulen, som vi vil skrive testene våre for. Så har vi definert en testfunksjon som kaller `greeting()`-funksjonen fra `Example` og bruker `assert.equal()` for å sjekke at outputen er lik "Hello World". Når du kjører testene, vil du få følgende resultat:

```
✓ Test passed: "Executing greeting() should return 'Hello World'"
```

## Dypdykk

Når du skriver tester i Gleam, er det viktig å forstå konseptet bak assert-funksjoner. Disse funksjonene brukes til å sjekke at en verdi er lik forventet verdi. Det finnes flere ulike assert-funksjoner som du kan bruke avhengig av hva du vil sjekke. For eksempel kan du bruke `assert.equal()` for å sammenligne to verdier, `assert.true()` for å sjekke om en betingelse er sann eller `assert.error()` for å fange feil i koden din.

Når du utvikler mer avanserte tester, kan du også dra nytte av konseptet med "mocking" og "stubbing" for å simulere ulike scenarier og verifisere at din kode reagerer riktig på disse scenariene. Modulen `gleam/mock` er tilgjengelig for å hjelpe deg med dette.

## Se også

- [Gleam dokumentasjon: Testing modul](https://gleam.run/book/stdlib.html#testing-module)
- [Enhetstesting med Gleam](https://blog.botreetechnologies.com/unit-testing-with-gleam-8f6f1b85f32c)
- [Mocking and Stubbing in Gleam](https://blog.botreetechnologies.com/mocking-and-stubbing-in-gleam-3611a7262b2a)