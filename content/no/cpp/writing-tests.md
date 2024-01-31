---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester er å lage kode som sjekker at annen kode fungerer som forventet. Programmerere gjør dette for å fange feil tidlig, sikre kvalitet, og forenkle vedlikehold.

## How to:
```C++
#define CATCH_CONFIG_MAIN  // La Catch ta seg av main-funksjonen.
#include "catch.hpp"       // Inkluder Catch2 testing framework header.

// Enkel funksjon vi vil teste.
int add(int a, int b) {
    return a + b;
}

// Vår test case.
TEST_CASE("Addition fungerer", "[matematikk]") {
    REQUIRE(add(2, 2) == 4);
    REQUIRE(add(-1, 1) == 0);
}
```
Kjør testene og sjekk output:
```plaintext
All tests passed (2 assertions in 1 test case)
```

## Deep Dive:
Tester i C++ spores tilbake til før årtusenskiftet med rammeverk som CppUnit. Alternativer til Catch2 inkluderer Google Test og Boost.Test. Effektiv testing krever en god forståelse av programkoden som testes og hva som er kritisk å teste. Unit tests isolerer og tester små kodebiter, mens integration tests sjekker hvordan disse bitene fungerer sammen.

## See Also:
- Catch2 dokumentasjon: https://github.com/catchorg/Catch2
- Google Test GitHub-side: https://github.com/google/googletest
- Boost.Test dokumentasjon: https://www.boost.org/doc/libs/release/libs/test/
