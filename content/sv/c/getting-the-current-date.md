---
title:    "C: Att få nuvarande datum"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att kunna hämta aktuellt datum är en viktig del av många programmeringsprojekt. Genom att inkludera aktuellt datum i ditt program kan du till exempel analysera data över en viss tidsperiod eller visa användare när programmet senast har uppdaterats.

## Hur man gör det

För att hämta aktuellt datum i C behöver vi använda standardbiblioteket `<time.h>`. Denna innehåller en mängd funktioner för att hantera tids- och datumdata.

Vi kan börja genom att deklarera en variabel av typen `time_t`. Detta är en typedef för en integer-typ som representerar tiden sedan "epoch" (1 januari 1970). Vi kallar sedan på `time()`-funktionen, som tar emot en pekare till en `time_t`-variabel som parameter och fyller den med aktuell tid.

```C
time_t current_time;
time(&current_time);
```

Vi kan sedan använda `localtime()`-funktionen för att konvertera `time_t`-variabeln till en `struct tm` som innehåller separata fält för år, månad, dag osv.

```C
struct tm *local_time = localtime(&current_time);

//Vi kan nu hämta det nuvarande året, månaden och dagen från structen och skriva ut det.
printf("Aktuellt datum: %d-%d-%d", local_time->tm_year + 1900, local_time->tm_mon + 1, local_time->tm_mday);
```

Output: Aktuellt datum: 2019-11-05

För att få mer precision kan vi också använda funktionen `gettimeofday()`, som ger oss aktuell tid i mikrosekunder.

```C
struct timeval current_time;
gettimeofday(&current_time, NULL);
printf("Aktuell tid: %ld mikrosekunder", current_time.tv_usec);
```

Output: Aktuell tid: 631724 mikrosekunder

## Djupdykning

För de som är intresserade av mer avancerad tidshantering finns det också möjlighet att använda funktionen `strftime()` för att konvertera tiden till en önskad strängformat. Det finns också möjlighet att använda `mktime()`-funktionen för att skapa en `time_t`-variabel från en `struct tm` med användardefinierade värden.

Se även

- Mer information om `<time.h>`: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- Utförlig dokumentation om time.h-funktionerna: https://linux.die.net/man/3/time
- En praktisk guide för att hantera tid i C: https://fresh2refresh.com/c-programming-tutorial/c-date-time/