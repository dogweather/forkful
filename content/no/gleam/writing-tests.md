---
title:                "Gleam: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Hvorfor skrive tester i Gleam

Skriving av tester er viktig for å sikre at koden din fungerer som den skal og for å oppdage eventuelle feil eller mangler. Dette gjelder spesielt for programmeringsspråk som Gleam, hvor funksjonelle egenskaper krever nøyaktighet og presisjon.

# Hvordan skrive tester i Gleam
For å skrive tester i Gleam må du først importere testbiblioteket ved å bruke `import gleam/test` kommandoen. Deretter kan du definere testene dine ved hjelp av `test` funksjonen og `assert` uttrykk. Her er et eksempel på en test som sjekker om to tall er like:

```Gleam
import gleam/test

pub fn lik(a, b) {
  test("to tall skal være like", {
    assert.equal(a, b)
  })
}
```

Når du kjører testen vil du se følgende output:

```
↗  to tall skal være like
    ✔  Ok
    ⟳
```

Dette betyr at testen din ble kjørt og at alt gikk greit, men hvis testen feiler vil du i stedet se et rødt kryss og en feilmelding.

# Dykke dypere
I Gleam er testing ansett som en viktig del av programmeringsprosessen. Du kan skrive så mange tester som du ønsker, og det anbefales å sjekke alle deler av koden din. Testene dine skal være så enkle og tydelige som mulig for å sikre at du tester koden din riktig.

En annen viktig del av Gleam-testing er å være sikker på at koden din oppfører seg som forventet ved feil eller ugyldig input. Dette kalles "property-based testing" og kan gjøres ved hjelp av biblioteket `gleam/quickcheck`. Dette biblioteket lar deg generere tilfeldig input og sjekke om din kode håndterer dem som forventet.

# Se også
- [Gleam-testbiblioteket](https://gleam.run/documentation/#testing-gleam-code)
- [Gleam QuickCheck-biblioteket](https://gleam.run/documentation/#property-based-testing)