---
title:    "C: Jämförelse av två datum"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av programmering eftersom det tillåter oss att utföra olika handlingar beroende på relationen mellan dessa datum. Det kan till exempel användas för att rapportera tidsskillnader, planera uppgifter eller kontrollera giltighetstiden för olika data.

## Hur man gör det

För att jämföra två datum i C finns det flera olika metoder att använda sig av. Vi kommer att fokusera på att använda strukturer och funktionen `difftime()` för att beräkna tidsskillnader.

Först måste vi definiera två variabler av typen `struct tm` som innehåller information om dag, månad, år etc. För att beräkna skillnaden mellan dessa två datum använder vi funktionen `difftime()` som tar emot två argument, det första är slutdatumet och det andra är startdatumet. Resultatet är antalet sekunder mellan dessa två datum.

```C
#include <stdio.h>
#include <time.h>

int main(void) {

  // Deklarera och definiera två variabler av typen struct tm
  struct tm startdatum = { .tm_year=2021, .tm_mon=9, .tm_mday=1 };
  struct tm slutdatum = { .tm_year=2021, .tm_mon=9, .tm_mday=5 };

  // Beräkna tidsskillnaden med hjälp av difftime()
  double skillnad = difftime(mktime(&slutdatum), mktime(&startdatum));
  
  // Skriv ut resultatet
  printf("Tidsskillnad: %.2f sekunder\n", skillnad);
  
  return 0;
}
```
Output:
```
Tidsskillnad: 345600.00 sekunder
```

## Djupdykning

Att jämföra datum med `struct tm` och `difftime()` är ett enkelt sätt att få reda på tidsskillnader. Det finns dock andra sätt att jämföra datum, till exempel med hjälp av `time_t` som representerar antalet sekunder sedan 1 januari 1970. Det finns också olika bibliotek som erbjuder mer avancerade funktioner för att hantera och jämföra datum.

## Se även

- Dokumentation för `struct tm`: https://www.cplusplus.com/reference/ctime/tm/
- Dokumentation för `difftime()`: https://www.cplusplus.com/reference/ctime/difftime/
- Tutorial för att jobba med datum och tider i C: https://www.tutorialspoint.com/cprogramming/c_date_time.htm