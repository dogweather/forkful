---
title:                "C: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å finne lengden på en tekststreng er en viktig del av programmering. Det tillater deg å behandle og manipulere tekst på en mer effektiv måte, og det er en grunnleggende ferdighet som alle programvareutviklere burde mestre. Så uavhengig av om du er en nybegynner eller en erfaren programmerer, er det viktig å forstå hvordan man finner lengden på en tekststreng.

# Hvordan

For å finne lengden på en tekststreng i C, bruker vi funksjonen `strlen()`. Denne funksjonen finnes i standard library for C, `string.h`. La oss se på et eksempel på hvordan man bruker denne funksjonen:

```C
#include <stdio.h>
#include <string.h>

int main ()
{
  char str[20] = "Hei, dette er en tekststreng";
  int lengde = strlen(str);
  printf("Lengden på teksten er %d", lengde);
  
  return 0;
}
```
Output:
```
Lengden på teksten er 28
```

Vi starter med å inkludere `string.h` i koden vår for å ha tilgang til `strlen()`-funksjonen. Deretter deklarerer vi en variabel `str` som inneholder teksten vi ønsker å finne lengden på. Vi bruker deretter `strlen()`-funksjonen på `str` og lagrer resultatet i variabelen `lengde`. Til slutt skriver vi ut resultatet ved hjelp av `printf()`-funksjonen.

# Dypdykk

Nå som vi har sett på en enkel måte å finne lengden på en tekststreng på, kan vi se litt nærmere på hvordan `strlen()`-funksjonen fungerer. Først og fremst, så er det viktig å huske at `strlen()` teller antall tegn i en tekststreng, ikke nødvendigvis antall ord. Dette betyr at den vil inkludere mellomrom og andre spesialtegn i beregningen. 

En annen ting å merke seg er at `strlen()` returnerer en `int`-verdi, som betyr at den har en grense på maksimalt antall tegn som kan telle. Dette kan være et problem hvis du trenger å finne lengden på en veldig lang tekststreng. I disse tilfellene, kan du bruke funksjonen `strlen_s()` som tar i bruk et større datatyper for å håndtere større verdier. 

# Se også

- [Hva er en streng i C](https://www.programiz.com/c-programming/c-strings)
- [Bruk av `string.h` library](https://www.geeksforgeeks.org/string-h-header-file-functions-c/)
- [En utfyllende guide til `strlen()`-funksjonen](https://www.geeksforgeeks.org/strlen-function-in-c/)