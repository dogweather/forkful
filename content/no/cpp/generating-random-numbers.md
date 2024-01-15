---
title:                "Generering av tilfeldige tall"
html_title:           "C++: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Skal du lage et spill med tilfeldig genererte hendelser og utfall? Eller kanskje du vil sikre at krypteringen din er så tilfeldig som mulig? Uansett hva du trenger tilfeldige tall til, er generering av slike tall en viktig del av programmeringsverdenen. I denne artikkelen vil vi se nærmere på hvordan man kan generere tilfeldige tall i C++.

## Hvordan
Generering av tilfeldige tall i C++ innebærer bruk av biblioteket <random>. Ved å inkludere dette biblioteket i koden din, vil du ha tilgang til ulike funksjoner og metoder for å generere tilfeldige tall. Her er et eksempel på hvordan du kan generere et tilfeldig tall mellom 1 og 10:

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd; // Oppretter et random device for å generere tilfeldige tall
    std::mt19937 gen(rd()); // Oppretter en Mersenne Twister-algoritme
    std::uniform_int_distribution<> dis(1, 10); // Oppretter en fordeling mellom 1 og 10
    int random_num = dis(gen); // Genererer et tilfeldig tall basert på fordelingen
    std::cout << "Tilfeldig tall mellom 1 og 10: " << random_num << std::endl; // Printer ut tilfeldig tall
    return 0;
}
```

Dette er bare et enkelt eksempel på hvordan man kan generere tilfeldige tall i C++. Det finnes også andre metoder som gir forskjellige fordelinger, som for eksempel normalfordeling eller eksponentialfordeling. Du kan også bruke seed-verdier for å få samme resultat hver gang du kjører koden.

## Dypdykk
Når man genererer tilfeldige tall i programmering, er det viktig å forstå at tallene egentlig ikke er helt tilfeldige. De er basert på en algoritme som bruker en startverdi, eller seed-verdi, for å generere tallene. Derfor kan man aldri garantere at tallene er helt tilfeldige, men man kan sikre at de er så nære tilfeldige som mulig ved å bruke avanserte algoritmer som Mersenne Twister.

Man kan også bruke tilfeldige tall til å generere tilsynelatende tilfeldige hendelser, men dette kan være vanskelig og krever mye ekstra kode. For eksempel, hvis man ønsker å generere et terningkast, kan man bruke funksjonen srand() for å sette en seed-verdi basert på klokkeslettet. Dette vil gi tilsynelatende tilfeldige terningkast hver gang koden kjøres.

## Se også
- [C++ Random Numbers](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Generating Random Numbers in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)