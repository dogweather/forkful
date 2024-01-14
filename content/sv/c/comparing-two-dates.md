---
title:    "C: Jämföra två datum"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra datum kan vara mycket användbart vid programmering, särskilt när man arbetar med program som hanterar tidsberoende data. Genom att jämföra datum kan du bestämma ordningen på händelser eller hantera tidsbegränsningar för ditt program.

Att förstå hur man jämför två datum kan spara tid och undvika felaktig funktionalitet i ditt program. Det är en grundläggande färdighet som alla C-programmerare bör ha.

## Så här gör du

Jämförelse av två datum i C kan göras med hjälp av det inbyggda biblioteket "time.h". Först måste du konvertera dina datum till ett "time_t"-värde. Detta värde representerar antalet sekunder som har gått sedan 1 januari 1970.

För att jämföra två datum kan du använda funktionen "difftime()" som finns i "time.h". Denna funktion tar två "time_t"-värden som argument och returnerar differensen i sekunder mellan dem.

En enkel kodexempel:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Konverterar datum till "time_t"-värden
    time_t date1 = mktime(&date1);
    time_t date2 = mktime(&date2);

    // Beräknar differensen i sekunder mellan de två datum
    double difference = difftime(date1, date2);

    // Kontrollerar om date1 är tidigare än date2
    if (difference > 0) {
        printf("date1 är senare än date2");
    } else if (difference < 0) {
        printf("date1 är tidigare än date2");
    } else {
        printf("date1 och date2 är samma datum");
    }

    return 0;
}
```

För detta kodexempel, om date1 är 5 sekunder senare än date2, kommer programmet att skriva ut "date1 är senare än date2".

## Djupdykning

Vid användning av "time.h"-biblioteket finns det några saker att vara medveten om vid jämförelse av datum. Till exempel hanterar funktionen "mktime()" inte skottår, vilket kan påverka beräkningar av datum som ligger under skottårsåret.

Det är också viktigt att notera att "time_t"-värden är beroende av systemets tidszon. Detta betyder att om ditt program körs på en annan dator i en annan tidszon, kan resultatet av jämförelsen av datum vara annorlunda.

## Se även

- Time and Date Functions in C: https://www.tutorialspoint.com/c_standard_library/c_function_time.htm
- Date Comparison in C: https://www.includehelp.com/c/compare-two-dates.aspx
- Creating a Date Variable in C: https://www.guru99.com/c-date-time-functions.html