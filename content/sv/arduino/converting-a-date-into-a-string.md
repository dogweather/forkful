---
title:                "Arduino: Omvandla ett datum till en sträng"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
När du arbetar med Arduino, kommer du troligtvis att stöta på behovet av att konvertera en datum till en sträng. Det kan vara för att skriva till en LCD-skärm, logga data eller helt enkelt för att spara information till en fil. Att veta hur man korrekt konverterar ett datum till en sträng är därför en viktig del av din Arduino programmeringskunskap.

## Hur man gör
För att konvertera ett datum till en sträng i Arduino, kan du använda funktionen `String()` tillsammans med `millis()`-funktionen, som genererar tiden sedan Arduino boardet startades. Här är ett exempel på hur du kan göra det:

```Arduino
unsigned long currentTime = millis(); // Hämtar tiden sedan Arduino boardet startades
String date = String(currentTime); // Konverterar tiden till en sträng
Serial.println(date); // Skriver ut strängen till seriell monitor
```

Kör detta program och du kommer att se en lång numerisk sträng på seriell monitorn. Men om du vill ha en specifik format för ditt datum, behöver du först konvertera din tid till ett läsbart datum och sedan använda `String()`-funktionen. Här är ett exempel på hur du kan göra det med hjälp av `day()/month()/year()` funktionerna:

```Arduino
unsigned long currentTime = millis(); // Hämtar tiden sedan Arduino boardet startades
unsigned long currentDate = currentTime/86400; // Konverterar tiden till antal dagar
int day = day(currentDate); // Hämtar aktuell dag
int month = month(currentDate); // Hämtar aktuell månad
int year = year(currentDate); // Hämtar aktuellt år
String date = String(day) + "/" + String(month) + "/" + String(year); // Skapar en sträng med formatet DD/MM/YYYY
Serial.println(date); // Skriver ut strängen till seriell monitor
```

Kör detta program och du kommer nu att se ett läsbart datum på seriell monitorn.

## Djupdyka
För att fördjupa dina kunskaper om att konvertera ett datum till en sträng i Arduino, kan det vara bra att undersöka vilka andra funktioner som kan vara användbara för att få det datumformat du vill ha. Till exempel kan du använda `hour()/minute()/second()` funktionerna för att lägga till tiden i ditt datum. Du kan också experimentera med olika sätt att formatera din sträng, som att lägga till olika separeringstecken eller ändra ordningen på dag, månad och år i formatet.

## Se också
Här är några resurser som kan hjälpa dig att fördjupa dina kunskaper om konvertering av datum till strängar i Arduino:

- [Officiell Arduino referens för millis() funktionen](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Officiell Arduino referens för String() funktionen](https://www.arduino.cc/reference/en/language/variables/data-types/stringconstructor/)
- [Tutorial: Date and time functions in Arduino](https://tronixstuff.com/2014/12/01/tutorial-date-and-time-functions-in-arduino/)
- [Arduino forumtråd om konvertering av datum till sträng](https://forum.arduino.cc/index.php?topic=221763.0)