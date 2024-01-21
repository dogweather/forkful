---
title:                "Jämföra två datum"
date:                  2024-01-20T17:32:38.718645-07:00
model:                 gpt-4-1106-preview
simple_title:         "Jämföra två datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum handlar om att bestämma deras ordning: är det ena före, efter eller samma dag som det andra? Vi gör detta för att hantera tidsbaserade händelser i program, som utgångsdatum eller schemaläggning.

## Så här gör du:
C erbjuder inte direkt stöd för datumjämförelse, men `time.h` kan hjälpa. Här är ett enkelt exempel på hur man jämför två datum:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0, 0, 0, 15, 4, 121}; // 15 maj 2021
    struct tm date2 = {0, 0, 0, 20, 4, 121}; // 20 maj 2021

    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    if (difftime(time1, time2) > 0) {
        printf("Datum 1 är efter Datum 2.\n");
    } else if (difftime(time1, time2) < 0) {
        printf("Datum 1 är före Datum 2.\n");
    } else {
        printf("Datumen är samma.\n");
    }

    return 0;
}
```

Detta ger följande utskrift:
```
Datum 1 är före Datum 2.
```

## Djupdykning:
I den tidiga C-eran hade vi inte `time.h`, och jämförelser blev mer komplicerade. Vi skulle hantera varje datumkomponent - år, månad, dag - manuellt. Med `time.h`, dock, omvandlar `mktime` en `struct tm` till ett `time_t`-värde, som representerar sekunder sedan "epoch" (1 januari 1970). `difftime` jämför sedan dessa sekunder.

Alternativen inkluderar databasfunktioner eller tredjepartsbibliotek som `date.h` i C++ för mer komplexa datumoperationer. Implementeringsdetaljer kan bli komplexa; tänk på skottår, tidszoner och kalenderomställningar.

## Se också:
- C Standard Library documentation for `time.h`: https://en.cppreference.com/w/c/chrono
- Howard Hinnant's Date library (för C++ användare): https://github.com/HowardHinnant/date
- Stack Overflow, en guldgruva för programmeringsfrågor: https://stackoverflow.com/questions/tagged/c+date