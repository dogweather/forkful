---
title:    "Arduino: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför 
Att kunna beräkna ett datum i framtiden eller i det förflutna är en viktig funktion inom Arduino programmering. Det kan hjälpa dig att skapa olika tidrelaterade projekt som t.ex. en tidsomräknare eller en påminnelse. 

## Hur man gör 
Följ stegen nedan för att lära dig hur man beräknar ett datum i framtiden eller i det förflutna med hjälp av Arduino. 

1. Börja med att definiera det aktuella datumet genom att använda `RTClib` biblioteket. Här är ett exempel på hur man gör det:

```Arduino
#include <RTClib.h> // inkludera biblioteket

RTC_DS3231 rtc; // skapa en instans av RTC_DS3231 klassen

void setup () {
  Serial.begin(9600);
  rtc.begin(); // starta RTC
}
```

2. Skapa variabler för år, månad och dag och använd `weekday()` funktionen för att få det nuvarande veckodagen. 

```Arduino
int year, month, day;

// få det nuvarande datumet
year = rtc.now().year();
month = rtc.now().month();
day = rtc.now().day();

// få det nuvarande veckodagen
int weekday = rtc.now().weekday();
```

3. Om du vill beräkna ett datum x antal dagar innan det nuvarande datumet, kan du göra det genom att minska värdet för dag med x. På samma sätt kan du lägga till x antal dagar genom att öka värdet för dag. Se koden nedan för att förstå hur man beräknar ett datum i det förflutna eller i framtiden.

```Arduino
// beräkna datumet 7 dagar innan det nuvarande datumet
day = day - 7;

// beräkna datumet 14 dagar efter det nuvarande datumet
day = day + 14;

// om värdet för dag är större än 31, justera månaden och nollställ värdet för dag
if (day > 31) {
  month ++;
  day = day - 31;
}

// om månaden är större än 12, justera året och nollställ månaden
if (month > 12) {
  year ++;
  month = month - 12;
}
```

4. Slutligen, använd `Serial.print()` för att skriva ut det beräknade datumet på seriell monitor. 

```Arduino
// skriv ut det beräknade datumet
Serial.print("Datumet är: ");
Serial.print(day);
Serial.print("/");
Serial.print(month);
Serial.print("/");
Serial.println(year);
```

## Djupdykning 
Att beräkna ett datum i framtiden eller i det förflutna kan vara lite komplicerat om man inte är bekant med hur minuter, timmar och dagar är relaterade till varandra. En månad kan vara antingen 28, 29, 30 eller 31 dagar och det händer att vissa månader har skottdagar. Det är viktigt att ha detta i åtanke när man beräknar ett datum, annars kan man få felaktiga resultat. 

## Se även
- [RTClib biblioteket](https://github.com/adafruit/RTClib)
- [Arduino referens för weekday() funktionen](https://www.arduino.cc/reference/en/language/functions/time/weekday/)
- [Onlineverktyg för att beräkna datum](https://www.timeanddate.com/date/duration.html)