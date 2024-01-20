---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Tilfeldige tallgenerering er prosessen med å produsere tall som ikke kan forutsis bedre enn ved tilfeldig sjanse. Programmerere gjør dette for mange formål, som å simulere data, implementere spill og til crypto.

## Hvordan:

Her er et grunnleggende eksempel på hvordan du genererer et tilfeldig tall i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(0)); 
    int num = rand(); 
    printf("Generated Random Number is: %d\n", num);
    return 0;
}
```
Når du kjører programmet, produserer det et tilfeldig tall hver gang.

## Dyp Dykk:

Tallgenerering med `rand()`-funksjonen i C har en interessant historisk kontekst. Denne funksjonen ble først introdusert i den opprinnelige ANSI C-standarden fra 1989 (C89). Derimot er verdien den returnerer ikke veldig tilfeldig, da den produserer en sekvens av tall som vil begynne å gjenta seg etter et visst antall kall.

Det er flere alternativer for å generere tilfeldige tall. I nyere C-standarder som C11, kan du bruke `rand_s()` eller `random()` for å få en bedre distribusjon av tilfeldige tall.

Når det gjelder implementeringsdetaljer, bruker `rand()` en pseudotilfeldig algoritme basert på en startverdi eller 'seed'. `srand(time(0));` brukes for å sette seed-verdien til det gjeldende systemtiden, noe som gir forskjellige resultater hver gang programmet kjøres.

## Se Også:

For en mer utfyllende studie av tilfeldige tallgenerering i C, ta en titt på følgende ressurser:


2. StackOverflow: [Why do I always get the same sequence of random numbers with rand() ?](https://stackoverflow.com/questions/4768180/why-do-i-always-get-the-same-sequence-of-random-numbers-with-rand)