---
title:                "Sammenlikning av to datoer"
date:                  2024-01-20T17:32:37.755260-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenlikning av to datoer"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Sammenligning av to datoer betyr å finne ut om en dato kommer før, etter, eller er den samme som en annen. Programmerere gjør dette for å håndtere tidsstyrte hendelser, gyldighetsperioder, eller rett og slett for å organisere data.

## How to:
C gir ikke innebygget støtte for dato-sammenligning, så vi bruker `time.h`-biblioteket for å konvertere datoer til `time_t`-type og sammenligne dem.

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm a, struct tm b) {
    time_t t_a = mktime(&a);
    time_t t_b = mktime(&b);

    if(t_a < t_b) return -1;
    if(t_a > t_b) return 1;

    return 0;
}

int main() {
    struct tm date1 = { .tm_year=122, .tm_mon=3, .tm_mday=15 }; // 15 April 2022
    struct tm date2 = { .tm_year=122, .tm_mon=4, .tm_mday=20 }; // 20 May 2022

    int result = compare_dates(date1, date2);

    if(result < 0) {
        printf("Date1 er før Date2\n");
    } else if(result > 0) {
        printf("Date1 er etter Date2\n");
    } else {
        printf("Datoene er like\n");
    }

    return 0;
}
```

Output:
```
Date1 er før Date2
```

## Deep Dive
Før `time.h`, måtte programmerere støtte seg på egen kode for å sammenligne datoer. Noen datasystemer bruker andre biblioteker eller tjenester for sammenligning, som `DateTime`-klassen i .NET. Når du implementerer sammenligning i C, bør du ta hensyn til tidssoner og sommertid hvis tiden er en del av dato-objektene.

## See Also
Ta en titt på disse ressursene for mer informasjon:
- ISO C Standard (ISO/IEC 9899) for detaljer om `time.h`: https://www.iso.org/standard/74528.html
- The Open Group Base Specifications Issue 7, 2018 edition (time.h): https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html
- C Date and Time tutorial fra Tutorialspoint: https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm
