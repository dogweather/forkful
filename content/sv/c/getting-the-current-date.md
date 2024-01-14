---
title:                "C: Att hämta aktuellt datum"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet kanske inte verkar som en stor uppgift, men det kan vara användbart i olika situationer som att lagra information eller skriva ut rapporter med datumet. I denna blogginlägg kommer vi att ta en titt på hur man enkelt kan få det aktuella datumet i ett C-program genom att använda biblioteksfunktioner.

## Hur man

För att få det aktuella datumet i C behöver vi först inkludera biblioteket time.h. Detta ger tillgång till funktioner som hjälper oss att arbeta med datum och tid.

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Skapar variabeln för att lagra datumet
  time_t datum;
  time(&datum);

  // Skriver ut datumet på skärmen
  printf("Aktuellt datum: %s", ctime(&datum));

  return 0;
}
```

I detta exempel använder vi funktionen ctime() för att konvertera den numeriska representationen av datumet till en sträng som kan skrivas ut. Resultatet beror på vilken datumformat som används på din dator.

Både time() och ctime() funktioner är en del av standardbiblioteket i C, så du behöver inte oroa dig för att installera eller importera ytterligare bibliotek.

## Djupdykning

Om du vill ha mer precision i hur datumet visas kan du använda funktionen localtime(). Detta returnerar en struct med specifik information om datumet, till exempel år, månad, dag och klockslag.

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Skapar en struct för att lagra datumet
  struct tm *datum_detaljer;
  time_t datum;
  time(&datum);

  // Använder localtime() och lagrar den i vår struct
  datum_detaljer = localtime(&datum);

  // Skriver ut komponenterna i datumet
  printf("Dagens datum är: %d/%d/%d", datum_detaljer->tm_year + 1900, datum_detaljer->tm_mon + 1, datum_detaljer->tm_mday);

  return 0;
}
```

Genom att använda de olika komponenterna som returneras från localtime() funktionen kan du anpassa hur datumet visas i ditt program.

## Se även

- [C Biblioteksfunktioner för datum och tid](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial på Hur man använder time.h i C](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)
- [Enkel guide för C-timing och datum](https://www.programiz.com/c-programming/c-current-date-time)