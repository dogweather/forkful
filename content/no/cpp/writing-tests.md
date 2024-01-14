---
title:    "C++: Å skrive tester"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive tester for koden din kan virke som en ekstra byrde i utviklingsprosessen, men det er en utrolig viktig og nyttig praksis. Ved å skrive tester, kan du sikre at koden din fungerer som den skal og fange eventuelle feil tidlig i utviklingsprosessen. Dette kan bidra til å spare tid og ressurser på lengre sikt.

## Hvordan

For å skrive tester i C++, kan du bruke et testrammeverk som for eksempel Google Test eller CppUnit. La oss se på et eksempel på hvordan du kan skrive en enkel test med Google Test:

```C++
#include <gtest/gtest.h>

TEST(AdditionTest, PositiveNumbers) {
  // Arrange
  int a = 5, b = 10;

  // Act
  int result = a + b;

  // Assert
  EXPECT_EQ(result, 15);
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
```

I dette eksempelet har vi definert en test for addisjon av positive tall. Vi starter med å inkludere Google Test-biblioteket, og deretter definerer vi testen vår ved hjelp av `TEST()`-makroen. Inne i testen har vi tre steg: `Arrange`, `Act` og `Assert`. Først setter vi opp starttilstanden for testen, deretter utfører vi den faktiske koden vi vil teste, og til slutt sjekker vi om resultatet er som forventet ved hjelp av `EXPECT_EQ()`-makroen. Til slutt initialiserer vi Google Test og kjører alle testene ved hjelp av `RUN_ALL_TESTS()`.

Når vi kjører dette eksempelet, vil vi få følgende output:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from AdditionTest
[ RUN      ] AdditionTest.PositiveNumbers
[       OK ] AdditionTest.PositiveNumbers (0 ms)
[----------] 1 test from AdditionTest (0 ms total)

[----------] Global test environment tear-down.
[==========] 1 test from 1 test suite ran. (4 ms total)
```

Som du kan se, blir testen vår kjørt og består uten problemer.

## Dypdykk

Det er mange forskjellige tilnærminger for å skrive tester i C++, og det finnes også flere forskjellige testrammeverk å velge mellom. Det er viktig å ta deg tid til å undersøke og velge den tilnærmingen og rammeverket som passer best for deg og ditt prosjekt. Det er også viktig å ha en god balanse mellom å skrive nok tester for å sikre kvaliteten på koden, men samtidig ikke bli overveldet av for mye testing.

En annen viktig ting å huske på er at det å skrive gode tester også handler om å skrive testbare og modulære koder. Dette vil gjøre det enklere å sette opp testing og fange eventuelle feil. Det kan også være lurt å integrere testing som en del av din kontinuerlige integrasjonsprosess, slik at du alltid har en pålitelig database med tester.

## Se Også

- [Google Test](https://github.com/google/googletest)
- [CppUnit](https://sourceforge.net/projects/cppunit/)
- [En guide til å skrive gode tester i C++](https://devblogs.microsoft.com/cppblog/unit-testing-in-cpp/)
- [Kontinuerlig integrasjon i C++-prosjekter](https://www.thoughtworks.com/continuous-integration-cpp)