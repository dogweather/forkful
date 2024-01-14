---
title:    "C++: Generering av tilfeldige tall"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver. Det kan brukes til å lage realistiske spill, teste algoritmer og mye mer. Les videre for å lære hvordan du kan generere tilfeldige tall med C++.

## Hvordan

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main() {
    // Grunnleggende eksempel på generering av tilfeldig tall
    int random_num = rand(); 
    std::cout << "Det tilfeldige tallet er: " << random_num << std::endl;

    // Generering av tilfeldige tall innenfor et bestemt intervall
    // I dette eksempelet vil det tilfeldige tallet bli mellom 1 og 10
    int random_range = rand() % 10 + 1;
    std::cout << "Det tilfeldige tallet innenfor intervallet er: " << random_range << std::endl;

    // Bruk av systemklokken for å generere et annet tall hver gang programmet kjøres
    srand(time(0)); 
    int random_time = rand() % 10 + 1; 
    std::cout << "Det tilfeldige tallet ved hjelp av systemklokken er: " << random_time << std::endl;
    return 0;
}
```

Eksempelutgang:

```
Det tilfeldige tallet er: 23194
Det tilfeldige tallet innenfor intervallet er: 7
Det tilfeldige tallet ved hjelp av systemklokken er: 5
```

## Dypdykk

C++ har mange funksjoner som kan brukes til å generere tilfeldige tall. En av de mest brukte er `rand()` funksjonen som returnerer et tilfeldig tall mellom 0 og `RAND_MAX`, som er en stor definert verdi. Ved å bruke modulo-operatør kan vi begrense dette tallet til et bestemt intervall.

Noen programmerere foretrekker å bruke en mer avansert tilfeldig tall generator som `mt19937` fra `<random>` biblioteket. Denne genererer et mer tilfeldig utvalg av tall sammenlignet med `rand()`.

Det er også verdt å merke seg at selv om tilfeldige tall kan virke "tilfeldige" for mennesker, er de faktisk generert gjennom bestemte algoritmer basert på et innsått "frø" tall. Dette frøet kan endres ved hjelp av `srand()` funksjonen for å få et annet sett med tilfeldige tall hver gang programmet kjøres.

## Se også

- [C++ Rand() funksjon](https://www.programiz.com/cpp-programming/library-function/cstdlib/rand)
- [C++ `<random>` bibliotek](https://www.learncpp.com/cpp-tutorial/59-random-number-generation/)
- [Random number generation in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)