---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utdrag av substrings er å hente en bestemt del av en streng. Dette er nyttig når programmerere trenger å jobbe med eller manipulere spesifikke deler av tekst, ikke hele tekststrengen.

## Hvordan gjør man det?

Her er et grunnleggende eksempel på hvordan vi kan trekke ut en substring fra en tekststreng i C.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hei, Verden!";
    char substr[10];

    memcpy(substr, &str[4], 6);
    substr[6] = '\0';
    
    printf("Utdraget substring er: '%s'\n", substr);
    return 0;
}
```

Kjører vi denne koden, vil utdata være:
```C
Utdraget substring er: 'Verden'
```

## Deep Dive

Historisk sett har behovet for å arbeide med deler av tekst blitt mer krevende med tiden. Tidligere språk hadde ikke innebygde funksjoner for å håndtere dette, men moderne språk som C gir oss funksjonene vi trenger.

Det finnes flere alternativer til `memcpy` for å hente ut substrings, som `strncpy`. Hver funksjon har sin egen unike implementering og bruksscenarier, så det er best å velge den som passer best for ditt spesifikke problem.

De viktigste detaljene i implementeringen er selve utdraget og avslutningen av den nye strengen med '\0'. Dette er viktig fordi uten '\0' på slutten kan vi ende opp med ekstra tegn i vår substring, som kan føre til all slags morsomme bugs.

## Se Også

For mer informasjon og alternative måter å håndtere substring utdrag, se følgende linker:

- [C Standard Library - string.h](https://en.cppreference.com/w/c/string)