---
title:                "C: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver, spesielt i datavitenskap og matematikk. Dette kan være nyttig for å simulere tilfeldige situasjoner eller for å generere unike identifikatorer.

## Hvordan

Det er flere måter å generere tilfeldige tall på i C-programmeringsspråket. En vanlig metode er å bruke funksjonen `rand()` som returnerer et tilfeldig tall mellom 0 og `RAND_MAX`. For å generere tall i et annet område, må man bruke matematiske formler som begrenser resultatet.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // Setter en forandrelig seed basert på nåværende tid
    srand(time(0));
    
    // Genererer 10 tilfeldige tall mellom 1 og 100
    for (int i = 0; i < 10; i++) {
        int random_num = rand() % 100 + 1;
        printf("%d\n", random_num);
    }
    
    return 0;
}
```

Eksempel output:
```
87
24
53
12
79
38
16
91
72
8
```

## Dykk dypere

I C er ikke `rand()` funksjonen nødvendigvis en pålitelig måte å generere tilfeldige tall på. Dette skyldes at den returnerer en sekvens av tall som vil gjenta seg selv etter en viss tid. Dette kan føre til sikkerhetsrisikoer i sensitiv informasjon som bruker genererte tilfeldige tall som passord eller krypteringsnøkler.

En bedre metode for å generere tilfeldige tall er å bruke funksjoner fra biblioteket `<stdlib.h>` som `random()` og `srandom()`, som garanterer mer komplekse og uforutsigbare tall for hver kjøring.

## Se også

- [The C Programming Language](https://en.wikipedia.org/wiki/The_C_Programming_Language)
- [Introduction to Random Numbers](https://www.geeksforgeeks.org/random-numbers-in-c/)