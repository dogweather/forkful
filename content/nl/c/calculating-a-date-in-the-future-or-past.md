---
title:                "Een datum in de toekomst of het verleden berekenen"
date:                  2024-01-28T21:55:33.502023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het berekenen van een toekomstige of verleden datum houdt in dat je de exacte dag moet uitvogelen die een specifiek interval verwijderd is van een bekende datum. Programmeurs doen dit voor het plannen van evenementen, het laten verlopen van tokens, herinneringen, enz.

## Hoe te:

Hier is direct bruikbare C-code om een datum in de toekomst te berekenen. We gebruiken `time.h` functies.

```c
#include <stdio.h>
#include <time.h>

int main() {
    time_t nu;
    struct tm nieuwe_datum;
    double dagenToeVoegen = 10; // 10 dagen in de toekomst

    // Haal de huidige tijd op en zet om naar tm struct
    time(&nu);
    nieuwe_datum = *localtime(&nu);

    // Voeg de dagen toe aan de huidige datum
    nieuwe_datum.tm_mday += dagenToeVoegen;
    mktime(&nieuwe_datum);

    // Output de nieuwe datum:
    printf("De datum over 10 dagen zal zijn: %02d-%02d-%04d\n",
           nieuwe_datum.tm_mday,
           nieuwe_datum.tm_mon + 1, // tm_mon is 0-11
           nieuwe_datum.tm_year + 1900); // tm_year is jaren vanaf 1900

    return 0;
}
```

Voorbeelduitvoer: `De datum over 10 dagen zal zijn: 12-04-2023`

## Diepgaande Duik

Terug in de tijd was het berekenen van toekomstige of vorige data een gedoe - geen ingebouwde functies, enkel puur algoritmisch plezier. Nu geeft C's `time.h` je `time_t`, `struct tm`, en functies zoals `mktime()` om het leven makkelijker te maken.

Alternatieven? Zeker. Voor complexe datum-tijd manipulatie gaan sommige ontwikkelaars voor bibliotheken zoals `date.h` voor C++ of de 'chrono' module.

De details? `mktime()` normaliseert `struct tm`. Dat betekent dat als je 40 aan dagen toevoegt, het overgaat in maanden, zelfs jaren. Goed om te weten, voordat je je eigen tijdmachine uitvindt die in cirkels ronddraait.

## Zie Ook

- C Standaardbibliotheek - `time.h`: https://en.cppreference.com/w/c/chrono
- Alternatieve datum- en tijd bibliotheken, zoals Howard Hinnant's `date.h` bibliotheek voor C++: https://github.com/HowardHinnant/date
- `mktime()` functie uitleg: https://www.cplusplus.com/reference/ctime/mktime/
