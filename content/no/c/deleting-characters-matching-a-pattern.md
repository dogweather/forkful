---
title:                "C: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster er en vanlig oppgave i programmering, spesielt når du arbeider med tekstbehandling. Det kan være nyttig å fjerne spesifikke tegn for å konvertere tekst til et annet format, eller for å rydde opp i uønskede karakterer i en tekststreng.

## Hvordan

Å slette tegn som matcher et mønster kan gjøres ved å bruke funksjoner som "strchr" eller "strpbrk" i C. Her er et eksempel å bruke "strchr" for å fjerne alle forekomster av et spesifikt tegn fra en tekststreng:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char string[] = "Hei, verden!";
  char *result;

  // Finn posisjonen til "e" i tekststrengen
  result = strchr(string, 'e');

  // Slett alle forekomster av "e" fra tekststrengen
  while (result != NULL) {
    memmove(result, result + 1, strlen(result));
    result = strchr(result, 'e');
  }

  printf("%s", string);

  return 0;
}
```

Output:
```Hei, vrdn!```

I dette eksemplet bruker vi "strchr" til å finne posisjonen til tegnet "e" i tekststrengen, og deretter bruker vi "memmove" til å flytte alle tegnene etter dette tegnet en plass fremover. Dette fjerner effektivt alle forekomster av tegnet "e" fra tekststrengen. 

## Dypdykk 

Det er flere måter å slette tegn som matcher et mønster på i C, og noen kan være mer effektive enn andre for bestemte situasjoner. Det er også viktig å være forsiktig med hvordan du manipulerer en tekststreng, da feil kan føre til uforutsette resultater eller til og med programkrasj. Det kan være lurt å bruke innebygde funksjoner som "strncpy" eller "strcpy_s" for å unngå bufferoverløp. 

## Se også

- [string.h dokumentasjon](https://www.cplusplus.com/reference/cstring/)
- [C string funksjoner tutorial](https://www.programiz.com/c-programming/c-strings)
- [Hvordan fjerne tegn fra en tekststreng i C](https://stackoverflow.com/questions/2022078/how-to-remove-characters-from-a-string-in-c)