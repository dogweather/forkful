---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:13:23.866348-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hämta det aktuella datumet innebär att få tag på dagens datum från systemets klocka. Programmerare gör detta för att logga händelser, utföra datumrelaterade beräkningar eller bara visa användaren vad det är för dag.

## Hur man gör:

Här är ett enkelt exempel på hur man får det aktuella datumet i C:

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);       // Hämtar nuvarande tiden som ett time_t värde
    struct tm *t = localtime(&now);  // Konverterar till lokal tid

    printf("Idag är det: %d-%02d-%02d\n", t->tm_year + 1900, t->tm_mon + 1, t->tm_mday);

    return 0;
}
```
Kör programmet och det kommer att visa dagens datum i formatet: ÅÅÅÅ-MM-DD.

## Fördjupning

`time.h` biblioteket har använts sedan de tidiga dagarna av C, och funktionerna `time` och `localtime` har varit standard sättet att hämta och hantera tid och datum. Det finns också alternativ som `gettimeofday` som ger högre precision men är mer komplicerad att använda. Om man behöver tid i andra tidszoner än den lokala använder man `gmtime` istället för `localtime`.

C11 introducerade en ny tidsbibliotek med `timespec_get`, vilket erbjuder en enkel metod att hämta tiden med nanosekunders precision. För många fall är dock 'time_t' och dess funktioner tillräckligt och fortsätter att vara populära på grund av sin enkelhet.

## Se även

För ytterligare information och resurser, kolla in dessa länkar:

- C Standardbiblioteket `time.h`: https://en.cppreference.com/w/c/chrono
- Linux manual för `time`: http://man7.org/linux/man-pages/man7/time.7.html
- C11 Standarden: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
