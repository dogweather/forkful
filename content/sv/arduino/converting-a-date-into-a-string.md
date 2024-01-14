---
title:    "Arduino: Omvandling av en datum till en sträng"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera ett datum till en sträng är en viktig del av många Arduino-projekt. Det kan hjälpa dig att visa datumet på en LCD-skärm, spara datumet i en datalog, eller använda det som del av ett meddelande i en kommunikationsmodul.

## Hur man gör

För att konvertera ett datum till en sträng i Arduino måste du använda funktionen `time_t to_tm(time_t t, struct tm *timezone)` från Time.h-biblioteket. Du behöver också ange en `time_t`-variabel som innehåller datumet som du vill konvertera, och en struktur för att lagra det konverterade datumet.

Här är ett exempel på hur du kan använda denna funktion för att konvertera dagens datum till en sträng och skriva ut det på serieporten:

```Arduino
#include <Time.h> // inkludera Time.h-biblioteket

time_t now = time(NULL); // skapa en time_t-variabel med dagens datum och tid
struct tm *local_time = gmtime(&now); // skapa en struktur för att lagra konverterade datumet
char date_string[30]; // skapa en array för att lagra strängen

to_tm(now, local_time); // konvertera datumet till en strukturerad tid
sprintf(date_string, "%d/%d/%d", local_time->tm_mday, local_time->tm_mon + 1, local_time->tm_year + 1900); // skapa en sträng med datumet och lagra den i en array

Serial.println(date_string); // skriv ut strängen till serieporten
```

I det här exemplet används `sprintf()`-funktionen för att formatera datumet i en sträng. I detta fall använder vi `%d` för att ange olika delar av datumet, såsom dag, månad och år. Men du kan även använda andra formatbeteckningar för att anpassa utseendet på din sträng.

## Djupdykning

För att förstå hur man konverterar ett datum till en sträng behöver du först förstå hur datum lagras i Arduino. Date and Time Library (Time.h) använder Unix-tid som standard för att spara datum och tid. Unix-tidpunkten representerar antalet sekunder som har gått sedan 1 januari 1970.

När vi använder funktionen `to_tm()` så konverteras Unix-tiden till en struktur med olika datum- och tidattribut, såsom år, månad, dag osv. Genom att använda formatbeteckningar som `%d` kan vi sedan skapa en sträng som representerar det konverterade datumet.

## Se även

- [Arduino Time Library dokumentation](https://www.arduino.cc/reference/en/libraries/time/)
- [Unix Time på Wikipedia (engelska)](https://en.wikipedia.org/wiki/Unix_time)
- [sprintf() funktionen på Arduino referenssidan](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)