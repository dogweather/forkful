---
title:                "C: Stor bokstavering av en streng"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Hvorfor bry seg med å gjøre en setning med små bokstaver til en med store bokstaver? Vel, det kan være nyttig når du for eksempel må gjøre en tittel mer iøynefallende eller lage en streng som skal brukes som en variabel eller kanskje til og med en passordbekreftelse.

## Hvordan gjøre det
For å gjøre en streng til store bokstaver i C, kan du bruke funksjonen `toupper()`. Her er et eksempel på hvordan den kan brukes:
```C
#include <stdio.h>
#include <ctype.h>
int main()
{
    char str[] = "Hei alle sammen!";
    int i = 0;
    while (str[i])
    {
        putchar(toupper(str[i]));
        i++;
    }
    return 0;
}
```
Dette eksemplet vil skrive ut "HEI ALLE SAMMEN!".

## Dypdykk
Selv om `toupper()` funksjonen er enkel og effektiv, er det viktig å merke seg at den kun fungerer for engelske alfabeter. Dersom du jobber med andre språk som har spesifikke store og små bokstavpar, så kan du bruke funksjonene `islower()` og `toupper()` i en kombinasjon for å håndtere disse tilfellene.

Det er også verdt å merke seg at `toupper()` funksjonen ikke endrer den opprinnelige strengen, men returnerer en kopi med store bokstaver. Dersom du trenger å endre den opprinnelige strengen, må du enten bruke `strcpy()` eller manuelt endre bokstavene i strengen.

## Se også
- [C library - cctype](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/toupper.htm)
- [Tutorialspoint - C library function - toupper()](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [JournalDev - C string library and its functions with examples](https://www.journaldev.com/31199/c-string-function-library-string-h)