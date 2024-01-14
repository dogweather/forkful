---
title:                "C++: Å skrive tester"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i C++ kan virke som en unødvendig og tidkrevende oppgave. Men i virkeligheten kan det være en svært nyttig praksis for å sikre kvaliteten og stabiliteten til koden din. Ved å skrive tester kan du oppdage feil og bugs i koden din tidlig, noe som sparer deg for mye hodebry og tid i fremtiden.

## Hvordan

Det første trinnet for å skrive tester er å inkludere en testramme i prosjektet ditt. En populær testramme for C++ er Google Test. Deretter kan du begynne å skrive tester ved å lage funksjoner som tester ulike deler av koden din. Her er et eksempel på hvordan du kan skrive en enkel test for å sjekke om to tall er like:

```C++
#include <gtest/gtest.h>

TEST(TestEqual, CheckEqual) {
  int a = 5;
  int b = 5;
  EXPECT_EQ(a, b); // forventer at a og b er like
}

int main(int argc, char** argv) {
  testing::InitGoogleTest(&argc, argv); // initialiserer testrammen
  return RUN_ALL_TESTS(); // kjører alle tester i prosjektet
}
```

Kjøringen av denne testen vil gi følgende output:

```
[==========] Running 1 test from 1 test case.
[----------] Global test environment set-up.
[----------] 1 test from TestEqual
[ RUN      ] TestEqual.CheckEqual
[       OK ] TestEqual.CheckEqual (0 ms)
[----------] 1 test from TestEqual (0 ms total)

[----------] Global test environment tear-down
[==========] 1 test from 1 test case ran. (0 ms total)
[  PASSED  ] 1 test.
```

Du kan se at testen har blitt kjørt og bestått siden de to tallene var like.

## Dypdykk

Det finnes mange forskjellige typer tester du kan skrive, som for eksempel enhetstester, integrasjonstester og ytelsestester. Det er viktig å velge den riktige typen tester for ditt spesifikke prosjekt. Ved å inkludere tester som dekker alle deler av koden din, kan du være sikker på at koden din fungerer som forventet og at eventuelle endringer ikke påvirker funksjonaliteten til andre deler av koden.

Å skrive gode tester tar tid og krever en grundig forståelse av koden din. Men det er en investering som definitivt er verdt det i det lange løp. Med gode tester kan du være trygg på at koden din vil fungere som forventet og at du har en solid grunnmur for eventuelle fremtidige endringer.

## Se også

- [Google Test dokumentasjon](https://github.com/google/googletest/blob/master/googletest/docs/primer.md)
- [12 beste C++ testrammer](https://www.descasio.com/blog/the-top-12-c-test-frameworks-for-unit-and-functional-testing/)