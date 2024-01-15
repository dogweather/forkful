---
title:                "Omvandla ett datum till en sträng"
html_title:           "Arduino: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Varför

Det finns många gånger när du behöver konvertera ett datum till en sträng för att kunna använda den i dina projekt. Det kan vara för att visa datumet på en LCD-skärm eller för att spara det i en fil. Oavsett anledningen är det bra att veta hur man gör denna konvertering för att kunna utnyttja datumen på bästa sätt.

# Hur man gör det

## Arduino-kodexempel

Först måste vi inkludera biblioteket som innehåller funktioner för datumkonvertering:
```Arduino
#include <RTClib.h>
```
Sedan behöver vi skapa ett objekt av typen `RTC_DS1307` för att kunna använda de inbyggda funktionerna:
```Arduino
RTC_DS1307 rtc;
```
Nu kan vi använda funktionen `now()` för att hämta det aktuella datumet och spara det i en `DateTime`-variabel:
```Arduino
DateTime currentDateTime = rtc.now();
```
För att konvertera datumet till en sträng använder vi funktionen `toString()`. Om vi vill ha datumet i formatet DD.MM.YYYY använder vi följande kod:
```Arduino
String dateString = currentDateTime.toString("DD.MM.YYYY");
```
Vi kan även välja att inkludera tiden i strängen genom att ändra formatet till "DD.MM.YYYY HH:MM:SS".

## Exempeloutput

Om vi nu skriver ut strängen på seriell monitor genom att använda funktionen `println()` får vi följande output:
```
24.08.2021
```
eller om vi inkluderar tiden:
```
24.08.2021 15:23:10
```

# Fördjupning

Det finns många olika format som du kan använda för att konvertera ett datum till en sträng. I exemplet ovan använde vi formatet "DD.MM.YYYY", men det finns andra tillgängliga format som kan vara mer lämpliga för ditt projekt. Du kan läsa mer om olika format i dokumentationen för RTClib.

En annan sak att tänka på är att om du vill använda strängen för att jämföra datum behöver du konvertera den till ett annat format, exempelvis Unix-tiden eller ett antal millisekunder. Detta görs enkelt med funktionen `unixtime()`, som finns tillgänglig i RTClib.

# Se även

- [RTClib dokumentation](https://github.com/adafruit/RTClib)
- [Översikt över olika format för konvertering av datum](https://en.cppreference.com/w/cpp/chrono/c/time_point/time_point_cast)
- [Funktionslista för DateTime-klassen](https://github.com/adafruit/RTClib/blob/master/RTClib.h#L112)