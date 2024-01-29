---
title:                "Twee datums vergelijken"
date:                  2024-01-28T21:57:07.540422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vergelijken van twee data draait allemaal om het uitzoeken van hun chronologie—zijn ze hetzelfde, is de ene eerder, of is de ene later? Programmeurs doen dit voor zaken als het sorteren van gebeurtenissen, het valideren van tijdvakken en het afhandelen van reserveringen. Het is alledaagse tijdwaarneming in code.

## Hoe:

In C gebruiken we vaak de `time.h` bibliotheek om met data om te gaan. Hier is een snel voorbeeld:

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // Omzetten naar time_t voor eenvoudige vergelijking
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    // Vergelijken
    if (t1 < t2) return -1; // date1 is eerder
    if (t1 > t2) return 1;  // date1 is later
    return 0;               // data zijn hetzelfde
}

int main() {
    // Twee data om te vergelijken
    struct tm date1 = { .tm_year = 120, .tm_mon = 5, .tm_mday = 14 }; // 2020-06-14
    struct tm date2 = { .tm_year = 122, .tm_mon = 11, .tm_mday = 3 };  // 2022-12-03

    int resultaat = compare_dates(date1, date2);

    if (resultaat < 0) {
        printf("Date1 is eerder dan Date2.\n");
    } else if (resultaat > 0) {
        printf("Date1 is later dan Date2.\n");
    } else {
        printf("Date1 is hetzelfde als Date2.\n");
    }

    return 0;
}
```

Voorbeelduitvoer:
```
Date1 is eerder dan Date2.
```

## Diepgaande Duik

Voordat `time.h` C zegende met gestandaardiseerde tijd functies, zou je jouw eigen datums vergelijkingen maken—riskant bedrijf met schrikkeljaren en al. Nu zijn `mktime()` en `time_t` de gangbare methoden. Ze handelen de grillen van kalenders af, zodat jij dat niet hoeft te doen.

`mktime()` neemt jouw `struct tm` datum, met al zijn gebruiksvriendelijke velden, en perst het in een `time_t` waarde. Deze waarde vertegenwoordigt seconden sinds het epoch (00:00, 1 januari 1970, UTC). Zodra jouw data in `time_t` zitten, is het gewoon een kwestie van nummervergelijking.

Er zijn chiquere alternatieven, zoals `difftime()` voor het vinden van het tijdverschil of het gebruik van bibliotheken van derden. Ze kunnen meer functies bieden, maar voor een eenvoudige vraag "Welke datum is eerder?" heeft de standaardbibliotheek meestal alles wat je nodig hebt.

Implementatie is afhankelijk van systeemtijdinstellingen—tijdzones en zomertijd kunnen je in de war brengen. `mktime()` interpreteert de `struct tm` als lokale tijd, dus wees bedachtzaam bij het vergelijken van data uit verschillende tijdzones.

## Zie Ook

- C `time.h` referentie: https://en.cppreference.com/w/c/chrono
- `time(7)` - overzicht van tijd en datum in Unix-systemen: http://man7.org/linux/man-pages/man7/time.7.html
- GNU C Library (glibc) handleiding over Tijd: https://www.gnu.org/software/libc/manual/html_node/Time.html
