---
title:                "C: Utvinning av understrenger"
simple_title:         "Utvinning av understrenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi jobber med tekstbehandling i C-programmering, kan vi finne oss selv i behovet for å trekke ut en del av en tekststreng, også kjent som en substring. Dette kan være nødvendig for å utføre forskjellige manipulasjoner på teksten, som å bytte ut en del av den med noe annet, eller å søke gjennom den for et bestemt tegn eller ord. Uansett årsaken, er det viktig å vite hvordan man kan ekstrahere substrings i C for å få jobben gjort.

## Hvordan

For å ekstrahere en del av en tekststreng i C, må vi bruke funksjonen `strnlen()` for å finne hvor mange tegn vi vil ha i substrings. Dette kan gjøres ved hjelp av `strlen()` funksjonen, som returnerer lengden på en tekststreng, og deretter trekke fra antall tegn på den første delen av tekststrengen vi ikke ønsker i substrings. For eksempel, hvis vi har en tekststreng "Hei alle sammen!", og ønsker å trekke ut "alle" delen, kan vi bruke `strnlen()` til å finne lengden på "Hei " og trekke fra dette fra total lengden.

Etterpå må vi bruke funksjonen `memcpy()` for å kopiere ønsket del av tekststrengen til en annen variabel. Denne funksjonen tar inn fire argumenter: målvariabel, kildevariabel, antall tegn som skal kopieres og startposisjon for å begynne å kopiere. For eksempel, hvis vi vil kopiere "alle" delen fra tekststrengen "Hei alle sammen!", vil vi bruke `memcpy()` funksjonen og angi målvariabel som en tom variabel, kildevariabel som tekststrengen, antall tegn som 4 (lengden på substring) og startposisjon som 4 (etter "Hei " delen).

Under er et eksempel på hvordan man kan implementere denne logikken i C med utgangen "alle":

```c
#include <stdio.h>
#include <string.h>

int main()
{
  char text[] = "Hei alle sammen!";
  char substring[5];
  int start_pos = 4;
  int len = strlen(text) - start_pos;

  memcpy(substring, &text[start_pos], len);
  substring[len] = '\0';
  printf("Output: %s", substring);
  return 0;
}
```

Output:
```
alle
```

## Dypdykk

Mens `strnlen()` og `memcpy()` funksjonene er de mest brukte for å trekke ut substrings i C, finnes det også andre metoder for å oppnå det samme resultatet. En annen løsning er å bruke `sscanf()` funksjonen som tillater oss å utføre en "scan" på en tekststreng og hente ut ønsket del basert på et gitt format. Dette kan være nyttig hvis vi ønsker å ekstrahere en del av en tekststreng basert på et bestemt tegn eller mønster.

Det kan også være lurt å lage en egen funksjon for å ekstrahere substrings, spesielt hvis dette er noe som vil bli brukt ofte i programmet ditt. Dette vil gjøre koden din mer lesbar og enklere å vedlikeholde.

## Se Også
- ["C String Functions"](https://www.programiz.com/c-programming/c-strings)
- ["How To Extract Substrings using C Programming Language"](https://www.techiedelight.com/extract-substring-from-string-c/)
- ["Using C Programming to Extract Substrings"](https://www.educba.com/substring-in-c/)