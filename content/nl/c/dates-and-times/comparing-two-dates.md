---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:49.577855-07:00
description: "Datums vergelijken in C houdt in dat wordt bepaald hoe de chronologische\
  \ relatie tussen twee datums is - of de ene datum voor de andere komt of dat ze\u2026"
lastmod: '2024-02-25T18:49:48.628423-07:00'
model: gpt-4-0125-preview
summary: "Datums vergelijken in C houdt in dat wordt bepaald hoe de chronologische\
  \ relatie tussen twee datums is - of de ene datum voor de andere komt of dat ze\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?

Datums vergelijken in C houdt in dat wordt bepaald hoe de chronologische relatie tussen twee datums is - of de ene datum voor de andere komt of dat ze hetzelfde zijn. Deze mogelijkheid is cruciaal in applicaties die te maken hebben met planning, deadlines of het bijhouden van gegevens, omdat het de organisatie en manipulatie van tijdgevoelige gegevens mogelijk maakt.

## Hoe te:

C heeft geen ingebouwd type voor datums, wat het noodzakelijk maakt om de `time.h` bibliotheek te gebruiken om met datum- en tijdsstructuren te werken. De `tm` structuur en de `difftime()` functie worden vaak gebruikt om datums te vergelijken. Hieronder staat een voorbeeld dat laat zien hoe je twee datums kunt vergelijken:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconden;

    // Eerste datum (YYYY, MM, DD)
    date1.tm_year = 2023 - 1900; // Jaar sinds 1900
    date1.tm_mon = 3 - 1;        // Maand [0-11]
    date1.tm_mday = 15;          // Dag van de maand [1-31]

    // Tweede datum (YYYY, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Omzetten naar time_t formaat
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Vergelijken
    seconden = difftime(time1, time2);

    if (seconden == 0) {
        printf("Datums zijn hetzelfde.\n");
    } else if (seconden > 0) {
        printf("Eerste datum komt na de tweede datum.\n");
    } else {
        printf("Eerste datum komt voor de tweede datum.\n");
    }

    return 0;
}
```

Uitvoer zou kunnen zijn:

```text
Eerste datum komt voor de tweede datum.
```

Dit programma initialiseert twee `tm` structuren met specifieke datums, zet ze om naar `time_t` formaat met behulp van `mktime()` en vergelijkt ze tenslotte met `difftime()`, dat het verschil in seconden (als een `double`) tussen de twee tijden retourneert.

## Diepgaande duik

In de vroege dagen van C vereisten datum- en tijdsoperaties handmatige berekeningen, waarbij vaak rekening werd gehouden met schrikkeljaren, het wisselende aantal dagen in maanden, en zelfs schrikkelseconden. De introductie van `time.h` in de ANSI C-standaard bracht standaardisatie in de tijdbehandeling in C, waardoor datum- en tijdoperaties werden vereenvoudigd.

Het gebruik van `time.h` voor datumvergelijking is eenvoudig maar heeft beperkingen. De `tm` structuur houdt geen rekening met tijdzones of zomertijd, en `difftime()` geeft alleen het verschil in seconden aan, en mist fijnere granulariteit voor bepaalde applicaties.

Voor applicaties die robuustere datum-tijdoperaties vereisen, inclusief ondersteuning voor tijdzones, overschakelingen naar zomertijd en nauwkeuriger tijdintervallen, bieden bibliotheken zoals `date.h` (een Howard Hinnant datum bibliotheek, geen onderdeel van de standaardbibliotheek) een modern alternatief voor `time.h`. Deze bibliotheken bieden meer uitgebreide hulpmiddelen voor datum-tijdmanipulatie in C++, profiterend van decennia van evolutie in programmeertaaldesign. Voor C-programmeurs blijft het gebruik van deze externe bibliotheken of het nauwkeurig omgaan met de complexiteiten van datum-tijdberekeningen direct noodzakelijk om nauwkeurige en cultureel bewuste datum-tijdmanipulatie te bereiken.
