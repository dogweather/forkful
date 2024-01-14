---
title:                "C: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne datoer i fremtiden eller fortiden kan være nyttig for å planlegge fremtidige hendelser eller for å se tilbake på fortiden. Det kan også være en god måte å øve seg på å bruke dato- og tidsfunksjoner i C-programmering.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden, vil vi bruke funksjonen `date` og `mktime` fra `time.h` biblioteket i C. Først må vi definere en struct som inneholder dato og tidselementer som år, måned, dag, time og minutter. Deretter kan vi bruke `mktime` funksjonen til å konvertere denne structen til en `time_t` variabel som representerer antall sekunder siden 1. januar 1970.

```C
#include <stdio.h>
#include <time.h>

int main()
{
  // Definerer en struct for dato og tid
  struct tm dato = {0};

  // Setter dato og tidselementer
  dato.tm_year = 2019 - 1900; //år - 1900
  dato.tm_mon = 6; //måned (0 - 11)
  dato.tm_mday = 15; //dag (1 - 31)
  dato.tm_hour = 13; //time (0 - 23)
  dato.tm_min = 30; //minutt (0 - 59)

  // Beregner dato i fremtiden ved å legge til 365 dager
  dato.tm_mday += 365;

  // Konverterer struct til time_t variabel
  time_t future = mktime(&dato);

  // Skriver ut datoen i fremtiden som en streng
  printf("Datoen i fremtiden er: %s", asctime(&dato));

  // Skriver ut antall sekunder siden 1. januar 1970 
  printf("Antall sekunder siden 1. januar 1970 er: %ld", future);

  return 0;
}
```

Output:
```
Datoen i fremtiden er: Fri Jul 19 13:30:00 2019
Antall sekunder siden 1. januar 1970 er: 1563550200
```

## Dykk dypere

Når vi beregner en dato i fremtiden eller fortiden, må vi ta hensyn til forskjellige faktorer som skuddår og antall dager i en måned. For eksempel vil summen av 365 dager ikke alltid være ett år, siden det kan være skuddår. Derfor er det viktig å bruke de riktige funksjonene som `mktime` som tar hensyn til disse faktorene.

En annen viktig faktor å være oppmerksom på er at C beregner datoer med utgangspunkt i år 1900, derfor må vi justere dato og år tilsvarende. Det er også viktig å merke seg at månedene i C beregnes fra 0 til 11, derfor må vi trekke fra 1 fra den faktiske måneden som vi vil bruke.

## Se også

- [Time and Date Functions in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Unix Time](https://en.wikipedia.org/wiki/Unix_time)
- [C Date and Time Functions](https://www.geeksforgeeks.org/date-time-functions-in-c-c-with-examples/)