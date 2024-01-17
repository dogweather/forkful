---
title:                "Jämförande av två datum"
html_title:           "Arduino: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum är en process som används för att avgöra vilket av två givna datum som är senare eller tidigare. Detta kan vara användbart för programmerare när man behöver sortera eller filtrera data baserat på datum. Det är ett vanligt förekommande problem inom programmering och kan lösas på olika sätt beroende på språk och plattform.

## Så här:
Det finns flera sätt att jämföra två datum i Arduino, men ett vanligt sätt är att använda funktionen ```time_t difftime(time_t t1, time_t t2)```. Detta returnerar antalet sekunder mellan de två givna tidpunkterna. Nedan följer ett exempel på hur du kan använda denna funktion för att jämföra två datum och utskriva resultatet i seriell monitor.

```
#include <Time.h>
#include <TimeLib.h>

void setup(){
  Serial.begin(9600);
  time_t date1 = now(); //hämtar nuvarande datum
  time_t date2 = date1 + (24*60*60); //lägger till en dag till date1 
  //omvandlar sekunder till datum
  Serial.print("Datum 1: ");
  Serial.println(asctime(localtime(&date1)));
  Serial.print("Datum 2: ");
  Serial.println(asctime(localtime(&date2)));
  Serial.print("Skillnaden i sekunder: ");
  Serial.println(difftime(date2, date1)); //jämför de två datum och skriver ut resultatet
}

void loop(){
  //ingen kod behövs för det här exemplet
}
```

När du laddar upp koden till ditt Arduino-kort och öppnar seriell monitor, kommer du se följande output:

```
Datum 1: Tue Dec 08 12:00:00 2020
Datum 2: Wed Dec 09 12:00:00 2020
Skillnaden i minuter: 86400
```

## Djupdykning:
Att jämföra två datum är ett vanligt förekommande problem inom programmering, och därför finns det många olika metoder och funktioner för att lösa det. En annan vanlig metod är att använda tidsstämplar, som är en numerisk representation av datum och tid. Det finns också bibliotek som kan göra mer avancerade jämförelser, som att kolla om ett datum kommer före, efter eller är samma som ett annat.

Det är också viktigt att vara uppmärksam på tidszoner och sommartid, som kan påverka resultatet av jämförelsen. Se till att ha korrekta inställningar för detta innan du utför din jämförelse.

## Se även:
- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- Arduino Date and Time functions: https://www.arduino.cc/en/Tutorial/BuiltInExamples/Time
- Time and Date Functions for Arduino: https://playground.arduino.cc/Code/Time/
- Time Comparison in Arduino: https://forum.arduino.cc/index.php?topic=394654.0