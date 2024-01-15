---
title:                "Utvinne delstrenger"
html_title:           "C: Utvinne delstrenger"
simple_title:         "Utvinne delstrenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å ekstrahere substringer i C? Vel, det er flere fordeler med å kunne gjøre dette. For det første kan det hjelpe deg med å håndtere store mengder data mer effektivt og på en enklere måte. For det andre kan det også bidra til å gjøre koden din mer lesbar og modulær.

## Hvordan gjøre det

Det er flere måter å ekstrahere substringer i C på, men vi skal se på de tre vanligste metodene: `strncpy()`, `memcpy()` og `sprintf()`. Her er et eksempel på hvordan du kan bruke `strncpy()` og `memcpy()`:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Denne setningen inneholder flere ord.";
  char dest[15];

  // Ekstraher et ord fra starten av setningen
  strncpy(dest, str, 5);
  dest[5] = '\0'; // Legg til termineringsnull
  printf("Første ord: %s\n", dest);

  // Ekstraher et ord fra midten av setningen
  memcpy(dest, str + 27, 6);
  dest[6] = '\0';
  printf("Midterste ord: %s\n", dest);

  return 0;
}
```

Output:

```
Første ord: Denne
Midterste ord: ord. 
```

Som du kan se, ekstraherer vi substringer ved å bruke funksjoner som kopierer en del av en streng til en annen variabel. Husk å inkludere `<string.h>`-biblioteket for å kunne bruke disse funksjonene.

For å ta en dypere dykk i dette emnet, kan vi også nevne funksjonen `sprintf()`, som lar deg formatere og kopiere en del av en streng til en annen. Her er et eksempel:

```C
#include <stdio.h>

int main() {
  char str[] = "Programmering er gøy!";
  char dest[30];

  // Ekstraher et ord fra starten av setningen
  sprintf(dest, "%s", str + 0);
  dest[15] = '\0'; // Begrens lengden på substrings til maks 15 tegn
  printf("Første ord: %s\n", dest);

  return 0;
}
```

Output:

```
Første ord: Programmering er 
```

Som du kan se, kan vi her også begrense lengden på substrings og dermed hente ut akkurat den delen av setningen vi ønsker.

## Dypdykk

Det er viktig å merke seg at disse metodene for å ekstrahere substringer ikke alltid er like effektive. For eksempel, hvis du ekstraherer en del av en streng som inneholder mange spesialtegn eller store mengder data, kan dette føre til problemer og feil i koden din. Derfor er det viktig å være klar over begrensningene og eventuelle konsekvenser ved å bruke disse metodene.

En annen ting å huske på er at når vi ekstraherer substringer, endrer vi også lengden på strengen. Dette kan forårsake problemer når vi senere skal jobbe med den originale strengen. Det er derfor viktig å være forsiktig og ta hensyn til slike situasjoner når du bruker disse metodene.

## Se også

- [How to Use Substrings in C](https://www.geeksforgeeks.org/how-to-use-substrings-in-c-programming/)
- [String handling functions in C](https://www.tutorialspoint.com/string-handling-functions-in-c-cplusplus)