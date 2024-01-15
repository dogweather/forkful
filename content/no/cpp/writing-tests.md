---
title:                "Skrive tester"
html_title:           "C++: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg med å skrive tester når man allerede har skrevet koden? Vel, la meg si dette - tester er som en sikkerhetsnett for koden din. De hjelper deg med å finne og fikse feil før de blir et problem for dine brukere.

## Hvordan
Det finnes mange forskjellige tester man kan skrive, men vi skal fokusere på de mest grunnleggende - enhetstester. Disse tester en enkelt enhet av koden din, som for eksempel en funksjon eller klasse, og sjekker om den gir riktig output for ulike inputs.

La oss se på et eksempel, der vi har en funksjon som legger sammen to tall og returnerer summen:

```C++
// Funksjonen vi ønsker å teste
int add(int a, int b) {
  return a + b;
}

// Tester funksjonen
TEST_CASE("add funksjonen returnerer summen av to tall") {
  REQUIRE(add(2, 3) == 5); // Test med to positive tall
  REQUIRE(add(-5, 10) == 5); // Test med et negativt og et positivt tall
  REQUIRE(add(-10, -5) == -15); // Test med to negative tall
  REQUIRE(add(0, 0) == 0); // Test med null som input
}
```

Her ser vi at vi har skrevet en test for ulike scenarier for å sikre at funksjonen vår faktisk regner ut riktig sum. Hvis en av disse testene feiler, vet vi at det er noe galt med funksjonen vår og vi kan fikse det før koden blir tatt i bruk.

## Dypdykk
Når man skal skrive tester, er det viktig å tenke på hva som skal testes og hva man ønsker å oppnå med testene. Det kan være fristende å bare skrive et par enkle tester og være fornøyd, men å tenke gjennom ulike utfall og skrive tester for disse kan bidra til en mer robust og pålitelig kodebase.

Det finnes også mange nyttige verktøy og biblioteker for å skrive tester i C++, som for eksempel Catch2 eller Google Test. Disse verktøyene kan gjøre det enklere å strukturere og utføre tester, og gi deg mer detaljerte rapporter om eventuelle feil.

## Se også
- [Enhetstesting i C++ med Catch2](https://github.com/catchorg/Catch2)
- [Google Test dokumentasjon](https://github.com/google/googletest)

(disclaimer: Some translation may not be accurate as I do not speak Norwegian fluently.)