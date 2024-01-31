---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester er prosessen der du lager kode for å sjekke at programkoden din fungerer som den skal. Programmerere gjør dette for å sikre at koden er feilfri, forenkle feilsøking og vedlikehold, og forbedre kodekvaliteten over tid.

## How to:
Enkel test med `assert`:
```C
#include <assert.h>

void testAddition() {
    int sum = 2 + 2;
    assert(sum == 4);
}

int main() {
    testAddition();
    return 0;
}
```
Output:
```
(Ingen output betyr suksess, et assert-krasj vil vise en feilmelding)
```

## Deep Dive
Tester i C startet med enkle `assert`-sjekker, før mer robuste rammeverk som Unity og CMock for TDD (Test-Driven Development) ble tilgjengelige. Disse rammeverkene tilbyr makroer og funksjoner for å lett sette opp, kjøre og rapportere testresultatene. Valg av testrammeverk avhenger ofte av prosjektets behov og kompleksitet.

## See Also
- [Unity Test Framework](http://www.throwtheswitch.org/unity)
- [CMock - Mocking Framework](http://www.throwtheswitch.org/cmock)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
