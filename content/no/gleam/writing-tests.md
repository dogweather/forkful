---
title:                "Skriving av tester"
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Tester er automatiserte sjekker for å sikre at koden gjør det den skal. Programmerere skriver tester for å unngå feil, spare tid og forbedre koden over tid.

## Hvordan:
Gleam-bruk:
```gleam
import gleam/should
import testy/your_module

pub fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn add_test() {
  should.equal(add(1, 2), 3)
}
```
Kjør testene med `gleam test`. Forventet output:
```
running 1 test
test add_test ... ok

test result: ok. 1 passed; 0 failed; 0 ignored
```

## Dypdykk:
Tester i Gleam stammer fra Erlang's robuste testingstradisjon. Alternativer som EUnit og Common Test har inspirert Gleam's enkle should-stil. Implementasjonsdetaljer i Gleam fokuserer på type-sikkerhet og vedlikeholdbarhet, noe som reflekteres i testbibliotekene.

## Se Også:
- Gleam's offisielle dokumentasjon om testing: https://gleam.run/book/tour/testing.html
- should biblioteket: https://hexdocs.pm/should/
- Eksempelsamling av Gleam tester: https://github.com/gleam-lang/example-projects
