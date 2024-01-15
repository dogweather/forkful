---
title:                "Omvandla ett datum till en sträng"
html_title:           "C: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är ett vanligt problem inom programmering. Det kan vara användbart för att presentera datum på ett visuellt sätt eller för att spara datum som en sträng i en databas. I den här artikeln kommer jag att förklara hur du kan göra det med hjälp av C.

## Så här gör du

För att kunna konvertera ett datum till en sträng i C behöver du använda funktionen `strftime()`. Detta är en inbyggd funktion i C som tillåter dig att formatera ett datum enligt en viss mall och spara det som en sträng. Här är ett exempel på hur du kan använda `strftime()`:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Skapa en struktur för datum och tid
    struct tm datum;
    // Ställ in datumet till 3 mars 2021
    datum.tm_year = 121; // år 2021 minus 1900
    datum.tm_mon = 2; // mars (månaden börjar på 0)
    datum.tm_mday = 3; // 3:e dagen i månaden
    // Anropa strftime för att formatera datumet
    char str[50]; // En sträng som lagrar datumet
    strftime(str, 50, "%d/%m/%Y", &datum); // Formatera till DD/MM/YYYY
    // Skriv ut strängen
    printf("Datumet är: %s\n", str);
    return 0;
}
```

Output:
```
Datumet är: 03/03/2021
```

Som du kan se i exemplet ovan kan du anpassa formatet för datumet genom att ändra på mallen som du skickar till `strftime()`. Här är några vanliga mallar:

- `%d` - Dag av månaden med ledande nolla
- `%m` - Månad med ledande nolla
- `%Y` - År med 4 siffror
- `%y` - År med 2 siffror
- `%H` - Timme (24-timmars format)
- `%I` - Timme (12-timmars format)
- `%M` - Minut
- `%S` - Sekund

Här är en komplett lista över alla mallar: [https://www.cplusplus.com/reference/ctime/strftime/](https://www.cplusplus.com/reference/ctime/strftime/)

## Deep Dive

Det finns många funktioner i C som kan hjälpa dig att konvertera ett datum till en sträng, men `strftime()` är den vanligaste. Det finns också en liknande funktion som heter `ctime()` som kan användas för att konvertera ett datum till en sträng på ett enklare sätt. En annan viktig aspekt att notera är att `strftime()` returnerar en `size_t` variabel, vilket bara är ett alias för en osignerad heltalstyp. Detta innebär att du kan använda funktionen för att räkna antalet tecken i den konverterade strängen.

## Se också

- [https://www.cplusplus.com/reference/ctime/strftime/](https://www.cplusplus.com/reference/ctime/strftime/)
- [https://www.cplusplus.com/reference/ctime/ctime/](https://www.cplusplus.com/reference/ctime/ctime/)
- [https://www.geeksforgeeks.org/how-to-convert-string-to-date-in-c/](https://www.geeksforgeeks.org/how-to-convert-string-to-date-in-c/)