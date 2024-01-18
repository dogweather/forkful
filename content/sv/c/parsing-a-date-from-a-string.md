---
title:                "Att analysera ett datum från en sträng"
html_title:           "C: Att analysera ett datum från en sträng"
simple_title:         "Att analysera ett datum från en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Parsing av datum från en sträng innebär att man extraherar datumet som finns inuti en textsträng och konverterar det till ett datum-objekt som kan hanteras av datorn. Programerare gör detta för att kunna utföra olika typer av beräkningar eller jämförelser med datumet.

# Hur man gör:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    char date_str[] = "2021-09-15"; // En sträng som innehåller ett datum
    struct tm parsed_date; // Skapa ett struct för att lagra det parsade datumet

    // Anropa funktionen strptime() för att parsea datumet från strängen
    // Formatet "%Y-%m-%d" beskriver hur strängen är uppbyggd
    strptime(date_str, "%Y-%m-%d", &parsed_date);

    // Nu kan vi använda funktionen mktime() för att konvertera det parsade datumet till en sekundrepresentation
    // Därefter kan vi få ut olika komponenter av datumet som år, månad, dag etc.
    // Använd %d för att printa ut som siffror och %c för att printa ut som bokstäver
    printf("Det parsade datumet är %d-%d-%d\n", parsed_date.tm_year + 1900, parsed_date.tm_mon + 1, parsed_date.tm_mday);
    printf("Året är %d\n", parsed_date.tm_year + 1900);
    printf("Månaden är %d\n", parsed_date.tm_mon + 1);
    printf("Dagen är %d\n", parsed_date.tm_mday);
    printf("Veckodagen är %c\n", parsed_date.tm_wday);
    printf("Dagen i året är %d\n", parsed_date.tm_yday);

    return 0;
}
```
Output:
```
Det parsade datumet är 2021-9-15
Året är 2021
Månaden är 9
Dagen är 15
Veckodagen är Wed
Dagen i året är 257
```

# Djupdykning:

Parsing av datum från en sträng är en vanlig uppgift inom programmering, särskilt när man arbetar med användardata eller data som hämtats från en textfil. Tidigare var parsing en manuell och besvärlig process som krävde mycket kod, men nu finns det inbyggda funktioner i många programmeringsspråk som underlättar detta.

Ett alternativ till att parsea datum från en sträng är att använda en strukturell regelbaserad parser, där man definierar mönster och regler för hur datumen måste se ut i strängen. Detta kan vara fördelaktigt om man behöver parsea datum från flera olika format.

I ovanstående kodexempel användes funktionerna strptime() och mktime() från standardbiblioteket time.h för att parsea och konvertera datumet. I strptime() betyder "%Y-%m-%d" att datumet är i formatet "ÅR-MÅNAD-DAG" och i mktime() krävs att man adderar 1900 till året och 1 till månaden för att få rätt värden.

# Se även:

- <a href="https://www.tutorialspoint.com/c_standard_library/time_h.htm">TutorialsPoint - time.h</a>
- <a href="https://www.programiz.com/c-programming/library-function/time.h/strptime">Programiz - strptime()</a>
- <a href="https://www.programiz.com/c-programming/library-function/time.h/mktime">Programiz - mktime()</a>