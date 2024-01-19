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

## Vad & Varför?
Konvertering av ett datum till sträng innebär omvandlingen av datumdata till textformat, vanligtvis för presentation eller lagring. Programmerare gör detta för att tillåta läsbarare hantering och visning av datat.

## Så här gör man:
Här är ett exempel på hur man konverterar ett datum till en sträng i Arduino.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600); 
  setTime(8, 30, 0, 1, 1, 2023); // Set the time to 8:30:00 on 1 Jan 2023.
}

void loop() {
  String datestr = "";
  datestr += day();
  datestr += " ";
  datestr += month();
  datestr += " ";
  datestr += year();

  Serial.println(datestr); // Outputs the date as a string.
  delay(1000);
}
```

## Djupdykning
Historiskt sett i tidiga programmeringssystem, kunde datum inte hanteras enkelt som strängar. Över tiden har olika system och språk utvecklat sina metoder för att ta itu med detta.

Alternativ till dessa metoder kan innebära att använda olika bibliotek som innehåller inbyggda metoder för datumsträngskonvertering, eller skapande av anpassade funktioner.

Detaljerad genomförande kan variera beroende på vilken version av Arduino du använder. Det kan också vara olika beroende på om du hanterar kalenderdatum (dag, månad, år) eller tid (timmar, minuter, sekunder).

## Se också
- För mer information om tidsfunktioner i Arduino, se: https://www.arduino.cc/en/Reference/Time 
- Ytterligare information om TimeLib-biblioteket: https://github.com/PaulStoffregen/Time 
- För en bredare kontext om datum och tidsformat, se: https://en.wikipedia.org/wiki/System_time.