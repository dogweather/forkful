---
title:    "C: Beräkning av ett datum i framtiden eller förfluten tid"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller det förflutna är en viktig färdighet inom programmering. Det kan vara användbart för att skapa kalendrar, planera händelser eller utföra olika tidsberäkningar i ett program.

## Hur man gör det

För att beräkna ett datum i framtiden eller det förflutna behöver vi använda oss av några grundläggande matematiska formler. Först och främst måste vi ha ett startdatum som vi utgår ifrån. Detta datum måste vara av typen `time_t`, vilket är en standardiserad datatyp inom C som representerar en specifik tidpunkt.

Efter att ha definierat startdatumet använder vi oss av ett `int` värde för att ange antalet dagar som vi vill addera eller subtrahera från vårt startdatum. Detta representerar alltså skillnaden mellan det framtida eller förflutna datumet och vårt startdatum.

I exemplet nedan har vi ett startdatum som är den 1 januari 2021 och vi vill räkna ut vilket datum det kommer att vara 30 dagar framåt i tiden.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    //Set start date
    time_t start_date;
    struct tm *start_tm;
    start_date = time(NULL);
    start_tm = localtime(&start_date);
    start_tm->tm_year = 121; //2021
    start_tm->tm_mon = 0; //January
    start_tm->tm_mday = 1; //1st
    start_date = mktime(start_tm);

    //Calculate future date
    int days_to_add = 30;
    start_date += days_to_add * 24*60*60; //Convert days to seconds and add to start date
    struct tm *future_tm;
    future_tm = localtime(&start_date);
    printf("The future date is: %d-%d-%d", future_tm->tm_year+1900, future_tm->tm_mon+1, future_tm->tm_mday);
    
    return 0;
}
```

Detta program kommer att skriva ut "The future date is: 2021-2-1" vilket betyder att datumet 30 dagar från och med 1 januari 2021 kommer att vara den 1 februari 2021.

## Djupdykning

För att kunna beräkna datum i framtiden eller det förflutna på ett korrekt sätt, måste vi ta hänsyn till olika faktorer som skottår och tidzoner. Därför är det viktigt att förstå hur `time_t` datatypen fungerar och hur man kan konvertera datum och tider mellan olika zoner.

Det finns också andra metoder för att beräkna datum i framtiden eller det förflutna, till exempel genom att använda sig av bibliotek som `datetime.h` eller `date.h` som innehåller mer avancerade funktioner för tidsberäkningar.

## Se även

- [C Programming Tutorial (på svenska)](https://www.programiz.com/c-programming)
- [Working with Dates and Time in C](https://www.guru99.com/c-date-time-operations.html)
- [The Time and Date Standard Library in C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)