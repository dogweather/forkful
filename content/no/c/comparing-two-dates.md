---
title:                "Sammenligner to datoer"
html_title:           "C: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor 
Å sammenligne to datoer kan være nyttig når man for eksempel jobber med å sortere eller filtrere data, eller når man ønsker å finne ut hvilken dato som kommer først eller sist. Å kunne sammenligne datoer er en viktig ferdighet å ha som programmerer, og det kan bidra til å gjøre koden din mer robust og effektiv. 

## Hvordan 
For å sammenligne to datoer i C, må man først konvertere dem til en datatype som kan sammenlignes, som for eksempel `time_t`. Dette kan gjøres ved hjelp av funksjoner som `mktime()` eller `strptime()`. Deretter kan man bruke operatorer som `<` eller `>` for å sammenligne datoene. Her er et enkelt eksempel:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Opprett to datoer
  struct tm dato1 = {31, 7, 2020};
  struct tm dato2 = {14, 2, 2021};

  // Konverter til time_t
  time_t tid1 = mktime(&dato1);
  time_t tid2 = mktime(&dato2);

  // Sammenlign datoene og skriv ut resultatet
  if (tid1 < tid2) {
    printf("Dato 1 kommer før Dato 2");
  } else {
    printf("Dato 2 kommer før Dato 1");
  }
  return 0;
}
```

Output:
```
Dato 1 kommer før Dato 2
```

Det er også mulig å sammenligne datoer ved å konvertere dem til en datostring og deretter bruke `strcmp()`-funksjonen til å sammenligne dem. 

## Dypdykk 
Når man sammenligner to datoer, må man være oppmerksom på at det kan oppstå feil på grunn av formatering eller ulikhet mellom tidssoner. Det er viktig å sørge for at datoene er konsistente og korrekt formatert før man sammenligner dem. Det kan også være lurt å bruke funksjoner som `difftime()` for å få differansen mellom to datoer i sekunder. 

En annen ting å være oppmerksom på er at å sammenligne datoer kan være ulikt på forskjellige plattformer eller operativsystemer. Det kan derfor være lurt å ta hensyn til dette når man utvikler kode som skal være plattformuavhengig. 

## Se Også 
- [How to Compare Dates in C](https://www.tutorialspoint.com/how-to-compare-dates-in-c-programming) 
- [Standard Library date and time utilities in C](https://en.cppreference.com/w/c/chrono) 
- [C Language reference](https://www.cplusplus.com/reference/clibrary/ctime/)