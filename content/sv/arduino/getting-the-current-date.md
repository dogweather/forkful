---
title:    "Arduino: Att få den aktuella datumet"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få ut den aktuella datumet kan vara användbart för många olika projekt som rör sig kring tidshantering. Genom att använda sig av Arduino och dess inbyggda RTC (real time clock) modul, kan vi enkelt få ut den nuvarande datumet direkt från vår kod. Det är även en grundläggande funktion för många olika applikationer som involverar tidgivning, så det är en bra funktion att ha kunskap om.

## Så här gör du

För att få ut den aktuella datumet i Arduino, behöver vi först ansluta en RTC-modul till vår Arduino. Detta gör vi genom att ansluta VCC till 5V, GND till jord och SDA/SCL till motsvarande pinnar på Arduino. Sedan behöver vi installera biblioteket "DS3231.h" för att kunna använda oss av RTC-funktionerna.

För att få ut datumet, använder vi oss av "RTC_DS3231" objektet och "now()" funktionen. Sedan kan vi använda följande kod:

```Arduino
#include <DS3231.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  rtc.begin();
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.day(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.year(), DEC);

  Serial.println();
}
```

Detta kommer att ge oss utskriften "DD/MM/YYYY" i seriell monitor. Vi kan även använda oss av funktioner som "now.hour()", "now.minute()" och "now.second()" för att få ut tidigare information.

## Djupdykning

Datumet som hämtas från RTC-modulen är i decimalformat, vilket betyder att en siffra som t.ex. "31" egentligen representerar det hexadecimala värdet "1F". Om du vill ha ut informationen som en "0"-fylld sträng, så finns det funktioner tillgängliga för detta. För att ta reda på vad de hexadecimala värdena betyder, kan du använda dig av hexadecimala kalkylatorer eller utför ett omvandlingsprogram.

## Se även

- DS3231 biblioteket: https://github.com/jarzebski/Arduino-DS3231
- Ytterligare resurser för att använda RTC på Arduino: https://www.instructables.com/id/Arduino-Real-time-clock-with-clock-sentence-outpu/