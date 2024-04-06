---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:06.443239-07:00
description: "Hvordan: En av de mest popul\xE6re tredjepartsbibliotekene for \xE5\
  \ skrive tester i C++ er Google Test. F\xF8rst m\xE5 du installere Google Test og\
  \ lenke den med\u2026"
lastmod: '2024-03-13T22:44:41.103478-06:00'
model: gpt-4-0125-preview
summary: "En av de mest popul\xE6re tredjepartsbibliotekene for \xE5 skrive tester\
  \ i C++ er Google Test."
title: Skrive tester
weight: 36
---

## Hvordan:


### Bruke Google Test Framework
En av de mest populære tredjepartsbibliotekene for å skrive tester i C++ er Google Test. Først må du installere Google Test og lenke den med prosjektet ditt. Når alt er satt opp, kan du begynne å skrive testtilfeller.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteNavn, TestNavn) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Lagre koden i en fil, og kompiler den med g++-kompilatoren, lenk Google Test-biblioteket. Hvis alt er satt opp riktig, vil kjøring av det resulterende eksekverbare programmet kjøre testen, og hvis `add`-funksjonen fungerer som forventet, vil du se noe som:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteNavn
[ RUN      ] TestSuiteNavn.TestNavn
[       OK ] TestSuiteNavn.TestNavn (0 ms)
[----------] 1 test from TestSuiteNavn (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Bruke Catch2
Et annet populært testrammeverk for C++ er Catch2. Det har en enklere syntaks og krever vanligvis ikke lenking mot et bibliotek (kun header). Her er et eksempel på hvordan du skriver en enkel test med Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Dette forteller Catch å tilby en main() - gjør dette kun i én cpp-fil
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Heltall multipliseres", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Når du kompilerer og kjører denne testen, gir Catch2 tydelig utdata som indikerer om testen ble bestått eller ikke, sammen med all informasjon som trengs for å feilsøke feil:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

Disse eksemplene viser hvordan integrering av testrammeverk i C++-utviklingsarbeidsflyten din kan forbedre påliteligheten og vedlikeholdbarheten av koden betydelig.
