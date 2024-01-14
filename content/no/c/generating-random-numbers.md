---
title:                "C: Generering av tilfeldige tall"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en nyttig ferdighet i programmering. Det tillater deg å lage dynamiske og varierte løsninger, enten det er for å simulere virkelige situasjoner eller for å sikre unike verdier i et spill. Uansett formål, er å kunne lage tilfeldige tall essensielt for enhver programmerer.

## Hvordan

Generering av tilfeldige tall i C er enkel takket være innebygde funksjoner i språket. Her er et eksempel på hvordan du kan generere en tilfeldig heltall mellom 1 og 100:

```C
#include <stdio.h> // inkluderer standard input-output biblioteket
#include <stdlib.h> // inkluderer standard biblioteket

int main()
{
    int tilfeldig = rand() % 100 + 1; // genererer et tilfeldig heltall mellom 1 og 100
    printf("Tilfeldig tall: %d\n", tilfeldig); // skriver ut det tilfeldige tallet
    return 0;
}
```

Kjører dette programmet flere ganger vil gi forskjellige tilfeldige tall hver gang. Dette er fordi funksjonen `rand()` genererer tall basert på en intern algoritme og den tilfeldige verdien vil endre seg hver gang programmet kjøres.

## Dypere dykk

Dette er det grunnleggende for å generere tilfeldige tall, men det finnes flere funksjoner i C som kan hjelpe deg å kontrollere og tilpasse den tilfeldige prosessen. For eksempel kan du bruke `srand()` for å initialisere en startverdi for den interne algoritmen, noe som kan føre til mer forutsigbare tilfeldige tall.

En annen viktig ting å huske på er at tilfeldige tall generert på denne måten ikke er helt tilfeldige i matematisk forstand. De følger en bestemt distribusjon og kan ikke garantere fullstendig tilfeldighet. For å få virkelig tilfeldige tall, er det nødvendig å bruke eksterne enheter som en tilfeldighetsgenerator eller en fysisk hendelse som en radioaktiv desintegrering.

## Se også

- [Generering av tilfeldige tall i C++](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Tilfeldighetsfunksjoner i C biblioteket](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Matematisk konsept av tilfeldig](https://no.wikipedia.org/wiki/Tilfeldighet)