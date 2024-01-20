---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att tolka ett datum från en sträng innebär att extrahera eller omvandla texten till ett datumformat. Programmerare gör detta för att behandla och utföra operationer med datum i sina program.

## Såhär gör du:

Här är ett exempel på hur du kan omsätta "DD-MM-YYYY" format till ett datumsobjekt med Arduino.

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  String dateString = "21-02-2021";
  int Day = dateString.substring(0,2).toInt();
  int Month = dateString.substring(3,5).toInt();
  int Year = dateString.substring(6,10).toInt();

  tmElements_t tm;

  tm.Day = Day;
  tm.Month = Month;
  tm.Year = Year - 1970;
  
  time_t t = makeTime(tm);
  Serial.println(year(t));
  Serial.println(month(t));
  Serial.println(day(t));
  delay(10000);
}
```

Programmet lista ut år, månad och dag från den ursprungliga strängen, omvandla dem till ett datum.


## Djupdykning

Historiskt sett handlar datumtolkning från strängar om att förstå och hantera olika datumformat som används globalt. Det finns olika sätt att göra detta i Arduino som att använda TimeLib biblioteket eller andra tredjepartsbibliotek. 

Användandet av `String.substring()` och `String.toInt()` funktioner är en av de mycket grundläggande sätten att tolka datum från strängar i Arduino. Dessa funktioner är inbyggda och behöver inte några extra bibliotek. Dock kan de vara begränsade när det gäller att hantera olika datumformat, tidzoner och skottår.

## Se även

- För mer info om 'Time' biblioteket, besök: [TimeLib bibliotek](https://www.pjrc.com/teensy/td_libs_Time.html)
- GitHub: [String.substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- GitHub: [String.toInt()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/toint/)
- För fler liknande exempel och tutorials, besök: [Arduino offiell hemsida](https://www.arduino.cc/)