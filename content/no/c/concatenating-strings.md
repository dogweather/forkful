---
title:                "C: Sammenslåing av strenger"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konkatenerere strenger er en viktig del av programmørens verktøykasse. Det gir muligheten til å kombinere flere tekststrenger og skape mer dynamiske og varierte utdata. Dette er spesielt nyttig når man jobber med brukerinput og formatering av tekst.

## Hvordan

For å konkatenerere strenger i C kan man bruke funksjonen `strcat()`. Denne tar inn to strenger og kombinerer dem til én. Her er et lite eksempel på hvordan dette kan gjøres:

```C
#include <stdio.h>
#include <string.h>

int main() 
{
    char navn[20] = "Jon";
    char etternavn[] = "Sørensen";

    strcat(navn, etternavn);

    printf("Navn: %s", navn);

    return 0;
}
```

Output: 
Navn: Jon Sørensen

Vi ser hvordan `strcat()` fungerer ved å kombinere strengene "Jon" og "Sørensen" til én streng. Det er viktig å merke seg at funksjonen endrer den originale strengen, og at det kan føre til feil om den kombinerte strengen blir for lang for det opprinnelige strengen.

## Dypdykk

Det finnes også andre metoder for å konkatenerere strenger i C, for eksempel å bruke `sprintf()` eller `snprintf()`. Disse funksjonene tar inn flere argumenter, inkludert formatet på strengen som skal kombineres. Det er også viktig å være klar over at man ikke kan konkatenerere heltall og strenger, dette må gjøres ved å konvertere heltallet til en streng først.

## Se også

- [C String Functions](https://www.programiz.com/c-programming/c-strings)
- [Concatenating Strings in C](https://www.geeksforgeeks.org/concatenate-strings-in-c-3-different-ways/)
- [Learn C Programming](https://www.learn-c.org/)