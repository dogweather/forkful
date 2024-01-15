---
title:                "Å finne lengden av en streng"
html_title:           "C: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Å finne lengden av en streng kan virke som en enkel oppgave, men det er faktisk et viktig konsept å forstå i programmering. Ved å forstå hvordan man finner lengden av en streng, kan man effektivt manipulere og behandle tekst i programmene sine.

##Hvordan
For å finne lengden av en streng i C, kan man bruke funksjonen ```strlen()```. Denne funksjonen tar inn en streng som parameter og returnerer antall tegn i strengen, uten å telle med null-terminator-tegnet som brukes for å markere slutten på en streng. La oss se på et eksempel:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char string[] = "Hei, verden!";
    int lengde = strlen(string);
    printf("Strengen \"%s\" har en lengde på %d tegn.", string, lengde);
    return 0;
}
```

Dette programmet vil skrive ut følgende:

```
Strengen "Hei, verden!" har en lengde på 12 tegn.
```

Vi kan også finne lengden av en streng ved å bruke en løkke for å telle antall tegn, men denne metoden er mer kompleks og mindre effektiv. Her er et eksempel:

```C
int lengde = 0;
while (streng[lengde] != '\0') {
    lengde++;
}
```

Som du kan se, må vi også telle med null-terminator-tegnet, og dette kan føre til feil dersom det blir glemt.

##Dypdykk
For å forstå hvordan funksjonen ```strlen()``` virker, er det viktig å vite hvordan strenger er representert i C. Strenger er egentlig bare en rekke med tegn, der den siste er null-terminator-tegnet. Når vi kaller på ```strlen()```, begynner den å telle fra starten av strengen og stopper når den kommer til null-terminator-tegnet.

Det kan også være nyttig å vite at funksjonen ```strlen()``` er definert i header-filen ```string.h```. Dette betyr at vi må inkludere denne filen i programmene våre for å bruke funksjonen.

##Se også
- [Offisiell C-dokumentasjon (engelsk)](https://devdocs.io/c/)
- [Lære C på 21 dager (engelsk)](https://www.tutorialspoint.com/cprogramming/index.htm)
- [W3Schools sin introduksjon til C (engelsk)](https://www.w3schools.in/c-tutorial/)