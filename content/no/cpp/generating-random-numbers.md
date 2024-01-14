---
title:                "C++: Generering av tilfeldige tall"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Å generere tilfeldige tall er en viktig del av programmering, og kan være nyttig i mange sammenhenger. Enten det er for å gi variasjon i et spill, lage unike brukernavn eller teste algoritmer, kan generering av tilfeldige tall være en svært nyttig funksjon.

# Hvordan gjøre det

For å generere tilfeldige tall i C++, kan vi bruke funksjonen `rand()`. Denne funksjonen genererer et tilfeldig tall mellom 0 og `RAND_MAX` (en konstant definert i `<cstdlib>` biblioteket). For å begrense området av tall som kan genereres, kan vi bruke modulus (restverdi) operatoren sammen med `rand()`. 

La oss si at vi vil generere et tilfeldig tall mellom 1 og 10. Vi kan da bruke følgende kode:

```C++
#include <cstdlib>
#include <iostream>

int main() {
    int random_num = std::rand() % 10 + 1;
    std::cout << random_num << std::endl;
    return 0;
}
```

I dette tilfellet genererer vi et tilfeldig tall mellom 0 og 9, og deretter legge til 1 for å få et tall mellom 1 og 10. Funksjonen `rand()` genererer alltid de samme tallene hver gang programmet kjøres, så vi må også bruke `srand()` funksjonen for å sette en ny startverdi for genereringen. For eksempel kan vi bruke systemklokken som å legge inn i `srand()` funksjonen, slik at den endrer seg hver gang programmet kjører.

```C++
#include <cstdlib>
#include <ctime>
#include <iostream>

int main() {
    std::srand(std::time(nullptr));
    int random_num = std::rand() % 10 + 1;
    std::cout << random_num << std::endl;
    return 0;
}
```

Slik kan vi generere et nytt tilfeldig tall hver gang programmet kjøres.

# Dypdykk

Å generere tilfeldige tall er ikke en helt tilfeldig prosess i seg selv. Det finnes ulike algoritmer som brukes for å generere tilfeldige tall, og noen av dem er mer "tilfeldige" enn andre. Noen av disse algoritmene bruker for eksempel fysiske fenomener som radioaktive atomer eller atmosfærisk interferens for å gi en mer tilfeldig generering.

I C++ er `rand()` funksjonen implementert ved bruk av en lineær kongruensgenerator. Dette kan føre til at genererte tall ikke er helt tilfeldige, og kan følge visse mønstre. Derfor er det viktig å bruke andre teknikker for å sikre mer "tilfeldige" tall, for eksempel ved å bruke eksterne tilførsler som nevnt tidligere.

# Se også

- [Random Number Generation in C++](https://www.learncpp.com/cpp-tutorial/random-number-generation/)
- [cstdlib - C++ Reference](https://www.cplusplus.com/reference/cstdlib/)
- [CppReference - rand()](https://en.cppreference.com/w/cpp/numeric/random/rand)