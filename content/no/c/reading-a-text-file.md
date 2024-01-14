---
title:    "C: Lese en tekstfil"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor
Text filer er et vanlig format for å lagre og dele data på en enkel måte. Det kan være nyttig for programmerere å kunne lese tekstfiler for å hente ut nødvendig informasjon eller manipulere data. I denne bloggartikkelen vil vi se nærmere på hvordan man kan lese en tekstfil ved hjelp av C-programmeringsspråket.

## Slik gjør du det
Før vi går inn i detaljene, la oss først se på en enkel måte å lese en tekstfil på. Vi kan bruke fopen() funksjonen for å åpne en tekstfil og deretter bruke fgets() funksjonen for å lese hver linje av filen. La oss se på følgende eksempel:

```C
#include <stdio.h>

int main(){
  FILE * fp;
  char line[100];

  // Åpne tekstfil for lesing
  fp = fopen("tekstfil.txt", "r");

  // Les hver linje av tekstfilen
  while(fgets(line, 100, fp) != NULL){
    printf("%s", line);
  }
  
  // Lukk filen
  fclose(fp);
  return 0;
}
```

I dette eksempelet åpner vi først en tekstfil med navnet "tekstfil.txt" for lesing (denne filen må selvfølgelig eksistere i samme mappe som programmet vårt). Deretter bruker vi en while-løkke for å lese hver linje av filen og skriver ut den til konsollen ved hjelp av printf() funksjonen. Til slutt lukker vi filen med fclose() funksjonen.

## Gå dypere
Når vi leser en tekstfil, kan det være nyttig å vite hvordan dataene er strukturert i filen. En tekstfil har en linje-struktur, noe som betyr at hver linje i filen er adskilt med et linjeskift. Dette linjeskiftet kan være representert som et nytt tegn ( '\n' ) eller et returtegn ( '\r' ). Det er viktig å huske på dette når du leser og manipulerer tekstfiler.

En annen viktig ting å vite er at fgets() funksjonen leser en linje av filen og returnerer en peker til denne linjen. Det betyr at vi må bruke en char-variabel til å lagre denne pekeren og deretter bruke denne variabelen for å få tilgang til linjen.

Nå som du har en grunnleggende forståelse av hvordan man leser en tekstfil, kan du prøve å eksperimentere med forskjellige funksjoner og metoder for å få ut enda mer informasjon fra tekstfiler.

## Se også
* [C-programmeringsspråket - Dokumentasjon](https://www.cprogramming.com/tutorial/c/lesson17.html)
* [Hvordan lese filer i C](https://www.programiz.com/c-programming/c-file-input-output)
* [C Library - fopen() funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)