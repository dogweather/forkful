---
title:                "C: Omvandla ett datum till en sträng"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

När man skapar ett program eller en applikation, är det ofta nödvändigt att hantera datum och tider. Ett vanligt problem är att konvertera ett datum till en sträng, vilket kan behövas för att presentera datum på ett läsbar sätt. Därför är det viktigt att ha kunskap om hur man konverterar ett datum till en sträng i C.

## Hur man gör det

Att konvertera ett datum till en sträng i C kan göras på flera olika sätt beroende på vilken format man föredrar för sin sträng. Ett enkelt sätt är att använda funktionen `strftime()`, som låter oss formatera datumet enligt önskemål. Nedan hittar du ett exempel på hur man kan använda denna funktion:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Definiera ett datum
  struct tm date = { .tm_mday = 29, // Dag
                     .tm_mon = 03, // Månad
                     .tm_year = 2021 - 1900}; // År

  // Konvertera datumet till en sträng
  char buffer[30];
  strftime(buffer, 30, "%d/%m/%Y", &date);

  // Skriv ut resultatet
  printf("%s", buffer);

  return 0;
}
```

Detta kommer att ge följande utskrift: `29/04/2021`.

Det är också möjligt att använda `sprintf()` för att konvertera datumet till en sträng, men detta kräver en något mer komplex kod. Här är ett exempel på hur man kan göra det:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Definiera ett datum
  struct tm date = { .tm_mday = 29, // Dag
                     .tm_mon = 03, // Månad
                     .tm_year = 2021 - 1900}; // År

  // Skapa en sträng för resultatet
  char result[30] = {0};

  // Konvertera datumet till en sträng
  sprintf(result, "%d/%d/%d", date.tm_mday, date.tm_mon, date.tm_year + 1900);

  // Skriv ut resultatet
  printf("%s", result);

  return 0;
}
```

Detta kommer att ge samma resultat som tidigare exempel.

## Djupdykning

När man konverterar ett datum till en sträng, är det viktigt att förstå vad de olika formattecken innebär. Till exempel betyder `%d` dag i månaden, medan `%m` betyder månad (med första siffran noll). För att få en komplett förståelse för alla möjliga formattecken, kan det vara bra att titta närmare på dokumentationen för `strftime()`. Det finns också andra funktioner som `asctime()` och `ctime()` som kan användas för att konvertera ett datum till en sträng.

## Se även

- C Datum funktioner: https://www.tutorialspoint.com/c_standard_library/c_date_time.htm
- Dokumentation för `strftime()`: https://www.cplusplus.com/reference/ctime/strftime/