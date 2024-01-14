---
title:    "C: Omvandling av en datum till en sträng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera datum till strängar är en viktig del av programmering eftersom det möjliggör att förstå och kommunicera datum på ett lättförståeligt sätt för både människor och datorer. Genom att omvandla datum till strängar kan vi till exempel visa datum på ett visuellt tilltalande sätt i en användargränssnitt eller spara datum i en databas.

## Hur man gör det
För att konvertera ett datum till en sträng i C-programmering, behöver vi använda några fördefinierade funktioner och formateringssekvenser. Den enklaste metoden är att använda funktionen `strftime()` som tar tre parametrar: en sträng för formatet, en buffert för resultatet och en `struct tm` variabel som innehåller datumet. Låt oss titta på ett exempel:

```C
#include <stdio.h>
#include <time.h>

int main(){
    time_t now;
    time(&now); //hämta aktuellt tidvärde
    char buffer[100];
    struct tm* timeinfo = localtime(&now);
    strftime(buffer, 100, "Idag är det lördagen den %d/%m/%Y.", timeinfo);
    printf("%s", buffer);
    return 0;
}
```

Detta kodexempel visar hur man kan konvertera dagens datum till en läsbar sträng med hjälp av `strftime()` funktionen. Resultatet blir "Idag är det lördagen den 12/06/2021." Det finns många olika formateringssekvenser som du kan använda för att anpassa utseendet på din sträng, såsom `%d` för dag, `%m` för månad och `%Y` för fullständigt årtal.

## Djupdykning
En stor del av förvirringen kring att konvertera datum till strängar beror på hur olika programmeringsspråk hanterar datum. Till exempel finns det skillnader i hur ett datum representeras och formateras mellan C och Java. Det är viktigt att förstå dessa skillnader för att kunna konvertera datum korrekt. I C används `struct tm` för att hantera datum, medan Java använder klassen `java.util.Date`, som sedan konverterar till `String` med hjälp av `SimpleDateFormat` klassen.

En annan viktig aspekt är tidszoner och hur de påverkar datum och tidsberäkningar. I C kan du använda `localtime()` funktionen för att konvertera till lokaltid, medan i Java behöver du använda `TimeZone` klassen och lägga till eller subtrahera antalet millisekunder beroende på tidszonen.

## Se också
- [strftime() function](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Formateringssekvenser i C](https://en.cppreference.com/w/c/chrono/strftime)
- [SimpleDateFormat class in Java](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Understanding date and time in C](https://www.programiz.com/c-programming/c-date-time)
- [Date and time handling in Java](https://www.baeldung.com/java-string-to-date)