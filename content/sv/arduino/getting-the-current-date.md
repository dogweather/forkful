---
title:                "Att få aktuellt datum"
html_title:           "Arduino: Att få aktuellt datum"
simple_title:         "Att få aktuellt datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att veta den aktuella datumen kan vara viktigt för att hålla ordning på olika projekt eller för att skapa tidsbaserade händelser i din Arduino-programmering. Med hjälp av en enkel kod kan du få Arduino att läsa av den aktuella datumen från en intern klocka och använda den informationen i ditt program.

## Så här gör du

För att få Arduino att hämta den aktuella datumen behöver du lägga till ett så kallat bibliotek i din kod. Biblioteket heter "Time" och är en del av Arduino-tidbaserad bibliotek. För att lägga till detta bibliotek i din kod öppna Arduino IDE och klicka på "Verktyg" och sedan "Bibliotek". I sökrutan skriver du "Time" och klickar på "Installera". Nu är biblioteket tillagt och du kan använda det i ditt program.

För att använda biblioteket behöver du inkludera det i din kod genom att skriva "Time.h" längst upp i koden. Nu kan du använda funktioner som getTimeStr() och getDayStr() för att få ut den aktuella datumen och konvertera den till strängar som du sedan kan använda i ditt program.

```
// inkluera biblioteket
#include <Time.h>

// få aktuellt datum som en sträng
String currentDate = getTimeStr();

// få aktuell veckodag som en sträng
String currentDay = getDayStr();

// skriv ut datumen till seriell monitor
Serial.println("Dagens datum: " + currentDate);
Serial.println("Veckodag: " + currentDay);
```

Output:
```
Dagens datum: 2020-10-19
Veckodag: måndag
```

## Deep Dive

Time-biblioteket bygger på en intern klocka i Arduino som håller koll på den aktuella datumen och tiden. Om du vill gå ännu djupare kan du läsa om funktionerna getTime() och getDate(), som ger dig datumen och tiden som separata variabler istället för som en sträng.

Det finns också möjlighet att ställa in en tidszon för den interna klockan, vilket kan vara användbart om du vill ha den aktuella datumen och tiden i en specifik tidszon. Det finns flera exempel på hur man gör detta i Time-bibliotekets dokumentation.

## Se också

- Time-dokumentation på Arduino hemsida: https://www.arduino.cc/reference/en/libraries/time/
- Time bibliotekets GitHub-repo med fler exempel och information: https://github.com/PaulStoffregen/Time