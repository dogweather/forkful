---
title:    "C: Å gjøre en streng stor bokstav"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Å kunne øke forståeligheten til teksten din ved å kapitalisere en streng er en viktig ferdighet i C-programmering. Det kan gjøre koden din mer lesbar og forståelig for andre utviklere, og bidra til å unngå feil og misforståelser.

## Hvordan

For å kapitalisere en streng i C, må du først definere en variabel som inneholder strengen du vil bruke. Deretter bruker du en for-løkke til å iterere gjennom hver bokstav i strengen. Inne i løkken kan du bruke standard C-funksjoner som toupper() eller en tilpasset funksjon for å endre hvert tegn til en stor bokstav. Til slutt skriver du ut den kapitaliserte strengen ved hjelp av printf()-funksjonen.

Et eksempel på hvordan koden kan se ut:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char string[] = "dette er en streng";
    int lengde = strlen(string);
    int i;

    for (i = 0; i < lengde; i++) {
        string[i] = toupper(string[i]); //kapitalisering av bokstav
    }

    printf("%s", string); //utskrift av den kapitaliserte strengen

    return 0;
}
```

Sample output:
Dette er en streng

## Dypdykk

I C finnes det flere måter å kapitalisere en streng på, avhengig av hva som skal gjøres med strengen etterpå. For eksempel kan du bruke en egen funksjon for å kopiere den kapitaliserte strengen til en ny variabel, slik at den originale strengen forblir uendret.

Det er også viktig å merke seg at C er et språk som skiller mellom små og store bokstaver, så du må være nøye med å bruke riktig funksjon for å endre bokstavene til store.

Det finnes også biblioteker og tredjepartsmoduler som kan bidra til å forenkle prosessen med å kapitalisere strenger, og som kan være nyttige å utforske.

## Se Også

- [Offisiell C-documentasjon for toupper() og andre funksjoner](https://www.cprogramming.com/reference/cctype/toupper.html)
- [Eksempel på en C-funksjon for å kapitalisere strenger](https://www.geeksforgeeks.org/program-uppercase-string/)
- [Biblioteker og moduler for å behandle tekster i C](https://www.tecmint.com/9-essential-string-manipulation-tools-in-c/)