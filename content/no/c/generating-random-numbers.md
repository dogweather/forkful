---
title:                "Generering av tilfeldige tall"
html_title:           "C: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Generering av tilfeldige tall er en viktig del av programmering som lar oss lage en slags "tilfeldighet" i våre programmer. Dette kan være nyttig for å forhindre forutsigbarhet og skape variasjon i resultatene.

# Hvordan:
Vi kan generere tilfeldige tall i C ved å bruke standardbiblioteket "stdlib.h". Her er et eksempel på hvordan vi kan generere et tilfeldig tall mellom 1 og 10:

```
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // initialiserer en variabel for å holde det tilfeldige tallet
    int tilfeldig_tall;

    // bruker funksjonen rand() fra stdlib.h for å generere et tilfeldig tall
    // begrenser også tallet til å være mellom 1 og 10 ved hjelp av modulus-operatoren
    tilfeldig_tall = (rand() % 10) + 1;

    // skriver ut det tilfeldige tallet
    printf("Det tilfeldige tallet er: %d\n", tilfeldig_tall);

    return 0;
}
```
Dette eksempelet bruker også "stdio.h" for å kunne skrive ut resultatet til konsollen. Kjører vi dette programmet flere ganger, vil vi se at det genererte tallet endrer seg hver gang.

# Dypdykk:
Generering av tilfeldige tall har vært en del av programmering siden de tidlige dager. I starten brukte man ofte fysiske fenomener som radioaktiv stråling eller støy for å lage tilfeldige tall. I moderne tid bruker vi derimot matematiske algoritmer for å generere disse tallene.

Det finnes også alternative metoder for å generere tilfeldige tall i C, som for eksempel å bruke tilfeldige bit-mønstre fra brukerinput eller fra operativsystemet. Dette kan være nyttig i visse tilfeller der man trenger en høy grad av tilfeldighet.

Implementasjonen av generering av tilfeldige tall kan variere mellom ulike datamaskiner og operativsystemer. Det er derfor viktig å være klar over eventuelle forskjeller dersom man trenger å generere tilfeldige tall for å kunne replikere resultatene på en annen maskin.

# Se også:
- Mer informasjon om rand() funksjonen i C: https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm
- Andre metoder for å generere tilfeldige tall i C: https://www.geeksforgeeks.org/generating-random-number-range-c/