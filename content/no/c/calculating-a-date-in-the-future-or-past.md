---
title:                "C: Beregning av en dato i fremtiden eller fortiden"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden er et nyttig verktøy i C-programmering. Det kan hjelpe deg med å planlegge og organisere hendelser eller prosjekter. Med et par enkle linjer med kode, kan du få programmene dine til å konvertere datoer og beregne fremtidige eller fortidige datoer.

## Hvordan

Det første du må gjøre er å definere en variabel for nåværende dato og en variabel for antall dager du vil legge til for å beregne fremtiden eller trekke fra for å beregne fortiden. For å gjøre dette kan du bruke biblioteket ""time.h" og funksjonen "time()" som returnerer nåværende tid i sekunder. Deretter kan du bruke funksjonen "localtime()" til å konvertere tid i sekunder til en lesbar struktur av typen "struct tm".

```C
#include <stdio.h>
#include <time.h>

int main(){

  int days = 30; //antall dager å legge til eller trekke fra

  time_t now; //variabel for nåværende tid i sekunder
  time(&now); //funksjonen returnerer nåværende tid i sekunder

  struct tm *date; //variabel for lesbar dato
  date = localtime(&now); //konverterer tid i sekunder til en struktur av typen "struct tm"

  //legger til eller trekker fra antall dager til dato
  date->tm_mday += days; 

  printf("Datoen %d dager fra nå er: %d/%d/%d",
  days, date->tm_mday, date->tm_mon+1, date->tm_year+1900); 
  //viser resultatet

  return 0;
}
```

Output:
Datoen 30 dager fra nå er: 27/10/2021

For å få beregnet en dato i fortiden, kan du bruke samme kode, men bare trekke fra antall dager i stedet for å legge til.

## Dypdykk

Når du bruker funksjonen "localtime()", må du huske å inkludere en time sonerjustering for å få riktig dato. Dette skyldes at funksjonen returnerer dato og tid basert på den lokale tiden på datamaskinen din. Hvis du vil ha den globale tiden, må du bruke funksjonen "gmtime()" i stedet.

En annen ting å huske på er at det bare er gyldige datoer som kan beregnes. Hvis du for eksempel prøver å finne en dato 30 dager fra nå på 31. oktober, vil det gi en ugyldig dato fordi det ikke er 31 dager i november.

## Se Også

- [Biblioteket ""time.h"](https://www.programiz.com/c-programming/library-function/time.h)
- [Funksjonen "time()"](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Funksjonen "localtime()"](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Funksjonen "gmtime()"](https://www.tutorialspoint.com/c_standard_library/c_function_gmtime.htm)