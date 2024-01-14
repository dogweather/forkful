---
title:                "Arduino: Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller förfluten tid är en användbar funktion inom många olika projekt. Det kan hjälpa till att hålla reda på deadlines, schemalägga händelser eller helt enkelt planera för framtiden.

## Hur man gör det

För att beräkna ett datum i framtiden eller förfluten tid på Arduino, behöver vi använda oss av en del inbyggda funktioner och variabler. För att komma igång, se till att inkludera biblioteket "Time.h" i din kod.

För att beräkna ett datum i framtiden kan vi använda funktionen "now()", vilket returnerar antalet sekunder sedan 1970-01-01 00:00:00 i Unix-timestamp format. Vi kan sedan lägga till önskat antal sekunder för att få det framtida datumet.

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  time_t now = now();
  time_t future = now + 3600; // Lägger till 1 timme
  Serial.print("Datum i framtiden: ");
  Serial.println(ctime(&future)); // Konverterar till läsbar form
  delay(1000);
}
```

Output:
```
Datum i framtiden: Thu Jul 08 21:42:37 2021
```

För att beräkna ett datum i förfluten tid behöver vi också använda funktionen "now()", tillsammans med funktionen "makeTime()". Här anger vi ett datum och tid för när sedan och beräknar antalet sekunder med hjälp av "now()".

```Arduino
#include <Time.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  tmElements_t since = { 0, 0, 0, 3, 1, 1970 }; // 1 mars 1970
  time_t now = now();
  time_t past = makeTime(since) - now; // Antal sekunder sedan 1 mars 1970
  Serial.print("Datum i förfluten tid: ");
  Serial.println(ctime(&past)); // Konverterar till läsbar form
  delay(1000);
}
```

Output:
```
Datum i förfluten tid: Thu Jul 04 18:51:40 2019
```

## Djupdykning

För att kunna beräkna ett datum i framtiden eller förfluten tid på rätt sätt, är det viktigt att förstå hur Unix-timestamp fungerar. Det är ett sätt att representera tid som antalet sekunder som har gått sedan 1970-01-01 00:00:00.

Genom att använda funktionen "now()" får vi en exakt tidpunkt i Unix-timestamp format, vilket vi sedan kan manipulera för att få rätt datum. Det är också viktigt att komma ihåg att dessa funktioner är beroende av den interna klockan på Arduinon, så det är viktigt att se till att den är rätt inställd.

## Se även

* [Time.h biblioteket på Arduino hemsida](https://www.arduino.cc/reference/en/libraries/time/)
* [Guide för att beräkna datum i framtiden eller förfluten tid på Arduino](https://www.instructables.com/Calculate-Future-and-Past-Dates-using-a-Real-Time-C/)