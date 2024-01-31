---
title:                "Een datum uit een string parsen"
date:                  2024-01-28T22:04:08.355048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het parsen van een datum uit een string betekent het extraheren en omzetten van de datum uitgedrukt als tekst naar een gestructureerd formaat dat een programma kan begrijpen en mee kan werken. Programmeurs doen dit omdat datums in tekstvorm niet handig zijn voor berekeningen, vergelijkingen, of om op te slaan in een gestandaardiseerd formaat.

## Hoe:

Hier is een kleine gids over het parsen van een datumstring in C met behulp van `strptime()` uit `time.h`. Het leest de datum in het formaat `"YYYY-MM-DD"` en zet deze om naar een `struct tm`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    const char *datum_str = "2023-03-14";
    struct tm tm;
    
    // Maak struct schoon om rommelwaarden te vermijden
    memset(&tm, 0, sizeof(struct tm));
    
    // Parse de datumstring
    if (strptime(datum_str, "%Y-%m-%d", &tm) == NULL) {
        printf("Parsen van datum mislukt.\n");
        return 1;
    }

    // Print de geparseerde datum
    printf("Jaar: %d, Maand: %d, Dag: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);

    return 0;
}
```

Voorbeelduitvoer:
```
Jaar: 2023, Maand: 3, Dag: 14
```

## Diepere Duik

Ooit waren datums een rommeltje om in C te hanteren, met programmeurs die handmatig strings parseerden door te rommelen met `strtok()`, `sscanf()` of zelfs rauwe lussen en karaktercontroles. Maar toen kwam `strptime()` binnen als onderdeel van POSIX, waardoor we strings die tijd voorstellen konden omzetten naar `struct tm` met vooraf gedefinieerde formaten.

Alternatieven zoals `getdate()` bestaan maar worden niet zo veel gebruikt. En dan is er nog de handmatige manier - het direct manipuleren van strings, maar laten we niet teruggaan naar die donkere tijden, oké?

Wat implementatie betreft, `strptime()` vereist wel dat je je `struct tm` opruimt omdat het dat niet voor je doet. Als je die nulstelling met `memset()` overslaat, kun je willekeurige rommel in de ongebruikte velden krijgen, wat leidt tot onverwachte resultaten.

Onthoud dat `strptime()` deel uitmaakt van POSIX, dus als je op een niet-POSIX systeem zoals Windows zit, moet je zoeken naar een andere oplossing of een compatibiliteitslaag, zoals `win32` implementaties of externe bibliotheken.

## Zie Ook

- [C++ `<chrono>` Bibliotheek](https://en.cppreference.com/w/cpp/header/chrono)
Voor degenen die ook in C++ duiken en op zoek zijn naar een modernere kijk op datum- en tijdsmanipulatie.

Hoewel de focus hier op C ligt, is een dieper begrip van POSIX-tijdfuncties altijd een pluspunt.

- [strftime en strptime Gedrag](https://man7.org/linux/man-pages/man3/strptime.3.html)
De manpagina voor `strptime()` en `strftime()` voor het begrijpen van hoe tijd te formatteren in C.

Als je speelt met tijd en datums, let op tijdzones en de veranderingen van zomertijd — deze kunnen roet in het eten gooien als ze niet goed worden afgehandeld. Veel programmeerplezier!
