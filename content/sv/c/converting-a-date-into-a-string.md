---
title:    "C: Omvandla ett datum till en sträng"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara användbart när man vill representera datumet på ett visuellt sätt eller när man behöver manipulera det på något sätt. Detta är särskilt viktigt i programutveckling när man hanterar olika datumformat och behöver konvertera dem för att kunna utföra olika operationer.

## Hur man gör det

En av de enklaste sätten att konvertera ett datum till en sträng i C är genom att använda funktionen `strftime()` från standardbiblioteket `time.h`. Den här funktionen tar tre parametrar: en buffert för den resulterande strängen, storleken på bufferten och ett format som specificerar hur datumet ska formateras. Här är ett exempel på hur man kan använda `strftime()`:

```C
#include <stdio.h>
#include <time.h>

int main() {
  struct tm *currentDate;
  time_t now;
  char buffer[80];

  time(&now);
  currentDate = localtime(&now);

  strftime(buffer, sizeof(buffer), "%d/%m/%Y", currentDate);
  printf("Idag är det %s\n", buffer);

  return 0;
}
```

I det här exemplet använder vi funktionen `localtime()` för att få det aktuella datumet och tiden och sedan använder vi `strftime()` för att formatera datumet till en sträng som har formatet "dag/månad/år".

Om du vill konvertera ett specifikt datum kan du istället använda funktionen `mktime()`, som omvandlar en `struct tm` till en `time_t` (en heltalstyp som representerar ett datum och tid i sekunder). Här är ett exempel på hur man kan använda `mktime()` tillsammans med `strftime()` för att konvertera ett specifikt datum till en sträng med ett annat format:

```C
#include <stdio.h>
#include <time.h>

int main() {
  struct tm date;
  time_t t;
  char buffer[80];

  date.tm_mday = 25; // dag 25
  date.tm_mon = 11; // december (tänk på att januari är 0)
  date.tm_year = 2021 - 1900; // 2021
  date.tm_hour = 0; // kl. 00:00:00
  date.tm_min = 0;
  date.tm_sec = 0;

  t = mktime(&date);

  strftime(buffer, sizeof(buffer), "%A %d %B %Y", localtime(&t));
  printf("Datumet är %s\n", buffer);

  return 0;
}
```

Det här exemplet konverterar datumet 25 december 2021 till en sträng med formatet "veckodag dag månad år". Om du vill experimentera med olika format kan du kolla in `strftime()` dokumentation och dess möjliga formatsträngar.

## Djupdykning

När man konverterar ett datum till en sträng är det viktigt att förstå olika tidszoner och hur dessa kan påverka resultatet. Standardbiblioteket `time.h` innehåller funktioner som `localtime()` och `gmtime()` som kan användas för att hantera tidszoner. `localtime()` använder den lokala tidszonen medan `gmtime()` använder UTC (Universal Time Coordinated) som standard. Det är också viktigt att förstå skillnaden mellan funktionerna `strftime()` och `ctime()`. Medan `strftime()` använder ett format för att formatera datumet till en sträng, använder `ctime()` standardformatet "dag mån dd hh:mm:ss år" som standard.

Att hantera datum och tider kan vara lite klurigt, särskilt i större projekt. Därför kan det vara användbart att använda externa bibliotek eller ramverk som är specialiserade på datumhantering, såsom `libstrfatus`. Dessa bibliotek kan erbjuda fler funktioner och mer flexibilitet än vad som finns tillgängligt i standardbiblioteket.

## Se även

- [Dokumentation för `strftime()`](https://linux.die.net/man/3/strftime)
- [Dokumentation för `ctime()`](https://linux.die.net