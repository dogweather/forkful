---
title:                "Å skrive tester"
html_title:           "C: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive tester er en viktig del av C-programmering fordi det hjelper deg med å identifisere og løse feil i koden din på en mer effektiv måte. Det gir også en god oversikt over hva koden din gjør og hvordan den fungerer.

## Hvordan du gjør det
Å skrive tester i C er enkelt og kan gjøres ved hjelp av et bibliotek som heter "CUnit". Her er et eksempel på hvordan du kan skrive en enkel test for en adderingsfunksjon:

```C
#include <stdio.h>
#include "CUnit/CUnit.h" // inkluderer CUnit-biblioteket

void test_add() {
    int result = add(2, 2); // kaller funksjonen som skal testes og lagrer resultatet
    CU_ASSERT_EQUAL(result, 4); // sjekker om resultatet er det forventede svaret
}

int main() {
    CU_initialize_registry(); // initialiserer testregistrering
    CU_pSuite suite = CU_add_suite("add_test_suite", NULL, NULL); // lager en test-suite
    CU_add_test(suite, "test_add", test_add); // legger til testen vår i suite-en
    CU_basic_run_tests(); // kjører testene
    CU_cleanup_registry(); // "rydder" etter testkjøringen og frigjør ressurser
    return 0;
}
```

Det ferdige resultatet vil være noe slikt:

```
CUnit - Enkle test ved bruk av asserts.

Suite: add_test_suite
  Test: test_add ...passed

Run Summary: Type    Total   Ran Passed Failed Inactive
              suites      1     1    n/a      0        0
              tests       1     1      1      0        0
              asserts     1     1      1      0      n/a

Elapsed time =   0.000 seconds
```

Som du kan se, ble testen vår kjørt og bestått. Du kan også legge til flere tester i samme suite og de vil bli kjørt etter hverandre. Det er også mulig å sette opp flere suites for å organisere testene dine på en bedre måte.

## Deep Dive
Når du skriver tester, er det viktig å dekke alle aspekter og mulige scenarier i koden din. Dette hjelper deg med å sikre at koden fungerer som den skal i alle tilfeller. Det er også lurt å teste både positive og negative tilfeller for å fange eventuelle feil som kan oppstå.

Ved hjelp av CUnit kan du også sette opp "fixture"-funksjoner som kjøres før og/eller etter hver test. Dette kan være nyttig for å sette opp felles variabler eller ressurser som trengs for flere tester.

Å skrive tester kan virke tidkrevende, men det er absolutt verdt det i det lange løp når du slipper å måtte feilsøke og rette opp i feil i koden din.

## Se også
- [CUnit dokumentasjon](https://cunit.sourceforge.io/)
- [En guide til å skrive tester i C](https://www.geeksforgeeks.org/testing-c-code/)