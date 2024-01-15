---
title:                "Å generere tilfeldige tall"
html_title:           "C: Å generere tilfeldige tall"
simple_title:         "Å generere tilfeldige tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor?

Å generere tilfeldige tall kan være nyttig i ulike programmeringsoppgaver og simuleringer. Det kan bidra til å skape variasjon og tilfeldighet i spill, generere unike nøkler for kryptering og være en del av algoritmer for å løse ulike matematiske problemer.

# Hvordan gjør man det?

For å generere tilfeldige tall i C, kan man bruke funksjonen `rand()` som finnes i standardbiblioteket `<stdlib.h>`. Denne funksjonen returnerer et tilfeldig tall mellom 0 og `RAND_MAX` (som er minst 32767). For å få et større tallområde kan man bruke funksjonen `srand()` for å sette en "seed" verdi, som vil påvirke tallene generert av `rand()`. Her er et eksempel på hvordan man kan generere 10 tilfeldige tall mellom 1 og 100:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    srand(1234); // Setter seed-verdien til 1234
    for (int i = 0; i < 10; i++) {
        int num = rand() % 100 + 1; // Tilfeldig tall mellom 0 og 100
        printf("%d ", num);
    }
    return 0;
}
```

```bash
Output: 42 85 20 89 66 54 26 76 18 98
```

# Dypdykk

Det finnes ulike metoder for å generere tilfeldige tall, og noen av disse kan være bedre egnet for ulike formål. For eksempel kan `rand()` følge et forutsigbart mønster, noe som ikke er ideelt for kryptografiske applikasjoner. I stedet bør man vurdere å bruke funksjoner som `random()` som bruker en mer kompleks algoritme for å generere tilfeldige tall. Det er også viktig å være oppmerksom på at "tilfeldige" tall generert av en datamaskin alltid vil være pseudo-tilfeldige og ikke virkelig tilfeldige. Det er derfor viktig å vurdere hvilken metode som er best egnet for formålet ditt.

# Se Også

- [C random number generation](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Generating random numbers in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Difference between rand() and random() in C](https://www.geeksforgeeks.org/rand-vs-random-functions-cpp/)