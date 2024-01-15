---
title:                "Stor bokstav i en streng"
html_title:           "C: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor?
I denne artikkelen skal vi se nærmere på hvordan du kan kapitalisere en streng i C-programmering. Dette er en nyttig ferdighet å ha hvis du for eksempel trenger å gjøre en streng i stor bokstav når du skal skrive ut noe, eller hvis du ønsker å sammenligne to strenger og ønsker at de skal være i samme format for å unngå feil.

## Hvordan gjøre det
Kapitalisering av en streng i C kan gjøres på flere forskjellige måter. Her er to enkle eksempler:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "dette er en streng";
    
    //Eksempel 1: Bruke en innebygd funksjon
    printf("%s\n", strupr(str)); //UTGANG: DETTE ER EN STRENG
    //strupr () er en innebygd funksjon i string.h biblioteket som konverterer en streng til store bokstaver
    
    //Eksempel 2: Bruke en løkke
    for (int i = 0; i < strlen(str); i++) {
        if (str[i] >= 'a' && str[i] <= 'z') {
            str[i] -= 32;
        }
    }
    printf("%s\n", str); //UTGANG: DETTE ER EN STRENG
    return 0;
}
```

Som du kan se, brukte vi en innebygd funksjon strupr () i det første eksempelet, mens vi i det andre eksempelet brukte en løkke og ASCII-koden for å konvertere fra små til store bokstaver.

## Dypdykk
Hvis du vil forstå mer om hvordan strenger blir behandlet og manipulert i C, er det nyttig å vite noen få ting om bruken av nullterminator (\ 0).

En streng i C er en sekvens med tegn som er lagret i minnet på en sammenhengende måte. Hver streng har en nullterminator som signaliserer slutten av strengen. Dette betyr at C vil fortsette å lese tegnene i strengen til den når \ 0-tegnet.

Når du kapitaliserer en streng ved å bruke en løkke, må du huske på å inkludere \ 0-tegnet som ellers vil føre til uønskede resultater. Det er også viktig å merke seg at ASCII-koden for store bokstaver ligger 32 tall over de små bokstavene. Derfor vil vi i det andre eksemplet trekke 32 fra verdien av små bokstaver for å få den tilsvarende store bokstaven.

## Se også
- [C programmeringspråk (Wikipedia)](https://no.wikipedia.org/wiki/C_(programmeringsspr%C3%A5k))
- [Kapitalisering av en streng i C (GeeksforGeeks)](https://www.geeksforgeeks.org/c-program-sort-string-without-using-string-function/)