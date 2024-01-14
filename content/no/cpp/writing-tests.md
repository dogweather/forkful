---
title:    "C++: Å skrive tester"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver C++ programmer er det viktig å sikre at koden fungerer som den skal. En måte å oppnå dette på er å skrive tester for å sjekke om koden gir ønsket resultat. Tester kan også hjelpe til å fange feil og gjøre det enklere å identifisere og fikse dem.

## Hvordan

For å skrive tester i C++, kan man bruke et test-rammeverk som for eksempel Boost.Test eller Google Test. Disse rammeverkene gjør det enkelt å skrive og kjøre tester.

La oss se på et eksempel på hvordan man kan skrive en test med Boost.Test:

```C++
#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(test_addition) {
    int result = 2 + 3;
    BOOST_CHECK_EQUAL(result, 5);
}
```

Her skriver vi en test som sjekker om 2+3 er lik 5. Hvis koden vår fungerer som den skal, vil denne testen passere. Hvis det skulle oppstå en feil, vil testen feile og gi beskjed om hva som gikk galt. Dette gjør det enklere å finne og fikse feil i koden.

Det er også mulig å sette opp flere tester og kjøre dem samtidig for å sikre at alle deler av koden fungerer som de skal.

## Dypdykk

Når man skriver tester er det viktig å huske å teste ulike scenarioer og grensetilfeller. Det kan være fristende å kun teste de vanlige situasjonene, men det er like viktig å teste for uventede verdier og input.

Det kan også være lurt å skrive tester før man implementerer koden, for å sikre at man får de ønskede resultatene og unngå å måtte gjøre store endringer senere.

Å skrive tester kan også bidra til å lage en mer strukturert og modulær kode, da man må tenke på hvordan man skal teste hver del av koden. Dette gjør det lettere å vedlikeholde og forstå koden i fremtiden.

## Se også

- [Boost.Test](https://www.boost.org/doc/libs/1_77_0/libs/test/doc/html/index.html)
- [Google Test](https://github.com/google/googletest)