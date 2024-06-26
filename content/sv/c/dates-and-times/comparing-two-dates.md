---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:43.394723-07:00
description: "Hur: C saknar en inbyggd typ f\xF6r datum, vilket n\xF6dv\xE4ndigg\xF6\
  r anv\xE4ndningen av `time.h`-biblioteket f\xF6r att arbeta med datum- och tidsstrukturer.\
  \ Strukturen\u2026"
lastmod: '2024-03-13T22:44:38.396301-06:00'
model: gpt-4-0125-preview
summary: "C saknar en inbyggd typ f\xF6r datum, vilket n\xF6dv\xE4ndigg\xF6r anv\xE4\
  ndningen av `time.h`-biblioteket f\xF6r att arbeta med datum- och tidsstrukturer."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## Hur:
C saknar en inbyggd typ för datum, vilket nödvändiggör användningen av `time.h`-biblioteket för att arbeta med datum- och tidsstrukturer. Strukturen `tm` och funktionen `difftime()` används ofta för att jämföra datum. Nedan är ett exempel som visar hur man jämför två datum:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double sekunder;

    // Första datumet (ÅÅÅÅ, MM, DD)
    date1.tm_year = 2023 - 1900; // År sedan 1900
    date1.tm_mon = 3 - 1;        // Månad [0-11]
    date1.tm_mday = 15;          // Dag i månaden [1-31]

    // Andra datumet (ÅÅÅÅ, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Konvertera till time_t-format
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Jämför
    sekunder = difftime(time1, time2);

    if (sekunder == 0) {
        printf("Datumen är samma.\n");
    } else if (sekunder > 0) {
        printf("Första datumet kommer efter det andra datumet.\n");
    } else {
        printf("Första datumet kommer före det andra datumet.\n");
    }

    return 0;
}
```

Output kan vara:

```text
Första datumet kommer före det andra datumet.
```

Det här programmet initierar två `tm`-strukturer med specifika datum, konverterar dem till `time_t`-format med `mktime()` och jämför dem slutligen med `difftime()`, som returnerar skillnaden i sekunder (som en `double`) mellan de två tidpunkterna.

## Fördjupning
I C:s tidiga dagar krävde datum- och tidsoperationer manuella beräkningar, ofta med hänsyn till skottår, den varierande antalet dagar i månaderna, och till och med skottsekunder. Introduktionen av `time.h` i ANSI C-standarden medförde standardisering till tidsbehandling i C, vilket förenklade datum- och tidsoperationer.

Att använda `time.h` för datumjämförelse är enkelt men har begränsningar. Strukturen `tm` tar inte hänsyn till tidszoner eller sommartid, och `difftime()` ger endast skillnaden i sekunder, och saknar finare granularitet för vissa applikationer.

För applikationer som kräver mer robusta datum-tidsoperationer, inklusive stöd för tidszoner, övergångar för sommartid och mer exakta tidsintervall, erbjuder bibliotek som `date.h` (ett Howard Hinnant-datum bibliotek, inte en del av standardbiblioteket) ett modernt alternativ till `time.h`. Dessa bibliotek ger mer omfattande verktyg för datum-tidsmanipulation i C++, och drar nytta av årtionden av utveckling i programmeringsspråksdesign. För C-programmerare kvarstår behovet av att använda dessa externa bibliotek eller noggrant hantera komplexiteten i datum-tidsberäkningar direkt för att uppnå precis och kulturellt medveten datum-tidsmanipulation.
