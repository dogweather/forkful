---
title:    "C: Å finne lengden av en streng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en veldig vanlig oppgave i C-programmering. Dette er ofte nødvendig for å håndtere tekst eller input fra brukeren. Det er også en god måte å øve på grunnleggende programmeringskonsepter på, som løkker og betingelser.

## Hvordan

Det finnes flere måter å finne lengden på en streng i C på. Den enkleste måten er å bruke `strlen` funksjonen fra standardbiblioteket. Her er en enkel kodebit som viser hvordan man kan bruke denne funksjonen:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // Definerer en streng
    char *tekst = "Dette er en streng";

    // Bruker strlen funksjonen for å finne lengden
    int lengde = strlen(tekst);

    // Skriver ut lengden
    printf("Lengden på strengen er %d", lengde);

    return 0;
}
```

Output:
```
Lengden på strengen er 20
```

Man kan også finne lengden på en streng ved å bruke en løkke og telle antall tegn. Her er en alternativ måte å finne lengden på samme streng som tidligere:

```C
#include <stdio.h>

int main()
{
    // Definerer en streng
    char *tekst = "Dette er en streng";

    // Initialiserer en teller
    int lengde = 0;

    // Løkke som teller antall tegn
    while (tekst[lengde] != '\0')
    {
        lengde++;
    }

    // Skriver ut lengden
    printf("Lengden på strengen er %d", lengde);

    return 0;
}
```

Output:
```
Lengden på strengen er 20
```

## Dypdykk

Når man bruker `strlen` funksjonen, vil den telle alle tegnene i en streng, inkludert mellomrom og spesialtegn. Det er viktig å være klar over dette når man manipulerer strenger i C, for å unngå uventede resultater.

Det finnes også alternative måter å finne lengden på en streng på. For eksempel kan man bruke `sizeof` operatøren på en streng for å finne størrelsen på den i bytes. Dette vil være det samme som å bruke `strlen`, men det kan være nyttig å vite om når man jobber med andre datatyper.

## Se også

* [Offisiell C-dokumentasjon for strlen](https://www.ibm.com/developerworks/library/l-cstring/)
* [En guide til strlen funksjonen](https://www.programiz.com/c-programming/library-function/string/strlen)