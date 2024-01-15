---
title:                "Beräkning av en datum i framtiden eller i historien"
html_title:           "Arduino: Beräkning av en datum i framtiden eller i historien"
simple_title:         "Beräkning av en datum i framtiden eller i historien"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Ibland behöver vi beräkna ett datum i framtiden eller i det förflutna för våra projekt. Det kan vara för att schemalägga viktiga händelser eller för att hålla koll på tidsbaserade processer.

## Hur man gör det

För att kunna beräkna ett datum i framtiden eller förflutna behöver vi använda oss av viss kod och matematik i vårt Arduino-program. Här är ett enkelt exempel som beräknar datumet för 3 månader framåt från dagens datum:

```Arduino
#include <TimeLib.h>

int dag = 9; // Ange dagens datum
int månad = 3; // Ange nuvarande månad
int år = 2021; // Ange nuvarande år

int månaderFram = 3; // Ange antal månader framåt eller bakåt

// Beräkna datum för 3 månader framåt
tmElements_t datum;
datum.Hour = 0; 
datum.Minute = 0;
datum.Second = 0;
datum.Day = dag;
datum.Month = månad + månaderFram;
datum.Year = år;
time_t t = makeTime(datum);
```

För att få ut det beräknade datumet kan vi använda följande kod:

```Arduino
int dagFram = day(t);
int månadFram = month(t);
int årFram = year(t);

Serial.print("Datumet tre månader framåt är: ");
Serial.print(dagFram);
Serial.print("/");
Serial.print(månadFram);
Serial.print("/");
Serial.println(årFram); 
```

Output för koden ovan skulle vara:

```
Datumet tre månader framåt är: 9/6/2021
```

## Deep Dive

För att kunna beräkna ett datum i framtiden eller förflutna behöver vi förstå hur tiden är strukturerad och lagrad i Arduino. I exemplet ovan använde vi biblioteket "TimeLib.h" som tillåter oss att arbeta med tidsberäkningar och funktioner som gör om tiden till ett mer läsbart format.

För att kunna göra beräkningen av ett datum använde vi funktionen "makeTime()" som tar in parametrar för årtal, månad, dag, timme, minut och sekund och omvandlar det till ett antal sekunder från 1 januari 1970 (Unix-epoch).

Det är också viktigt att komma ihåg att vi behöver inkludera "TimeLib.h" i början av vårt program för att kunna använda dess funktioner. Mer information om hur man använder "TimeLib.h" hittar du på dess dokumentationssida.

## Se även

* [TimeLib.h dokumentation](https://www.arduino.cc/en/reference/due3): Mer information om hur man använder biblioteket "TimeLib.h".
* [Kalenderräknare för Arduino](https://playground.arduino.cc/Code/DateTime/): Ytterligare exempel på hur man kan använda tid i Arduino-programmering.