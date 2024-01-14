---
title:                "Arduino: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna ett datum i framtiden eller det förflutna kan vara användbart för olika projekt, såsom att bygga en kalenderfunktion eller skapa en påminnelseapp. Med hjälp av Arduino kan du enkelt programmera och köra dessa beräkningar med din egna hårdvara.

## Så här gör du

För att beräkna datumet i framtiden eller det förflutna på Arduino, måste du först förstå hur datumet är uppbyggt. Standardformatet för datum är "åååå-mm-dd", med år, månad och dag som separata värden. För att beräkna ett datum behöver vi också veta antalet dagar i en månad samt om det är ett skottår eller inte.

För att få dagens datum kan vi använda funktionen `now ()` som ger oss antalet sekunder sedan starten av Arduino. Detta värde kan sedan omvandlas till dagar genom att dela det med antalet sekunder på en dag. Sedan behöver vi bara lägga till eller dra bort det önskade antalet dagar för att få det rätta datumet.

Låt oss säga att vi vill beräkna datumet 10 dagar framåt från idag. Med hjälp av koden nedan kan vi göra det:

```Arduino
#include <Time.h> // för att använda tidsfunktioner
int daysToAdd = 10; // antal dagar vi vill lägga till 
unsigned long now_seconds = now(); // antal sekunder sedan Arduino startades
int today = now_seconds / 86400; // antal dagar som har gått sedan starten (86400 är antalet sekunder på en dag)
int future_date = today + daysToAdd; // adderar antal dagar till dagens datum
Serial.println(future_date); // skriver ut det framtida datumet
```

Kör du koden kommer du att få ut datumet som antal dagar från start (dagens datum). För att få ut datumet i standardformatet "åååå-mm-dd" måste du konvertera det till år, månad och dag med hjälp av en omvandlingsfunktion.

## Djupdykning

Som nämnts tidigare är det viktigt att veta antalet dagar i en månad för att kunna beräkna datumet korrekt. Men i vissa fall, som vid skottår, är det också nödvändigt att ta hänsyn till extra dagar. Detta görs genom att kolla om året är delbart med 4 och i så fall är det ett skottår, utom om året också är delbart med 100, då är det inte ett skottår. Om året dessutom är delbart med 400, är det ändå ett skottår.

Det finns också andra sätt att beräkna datum i förflutna eller framtiden baserat på specifika händelser eller årliga evenemang. Dessa metoder kan vara användbara för projekt som involverar till exempel kalendrar eller påminnelser.

## Se även

- [Arduino dokumentation om tidsfunktioner](https://www.arduino.cc/reference/en/libraries/time/)
- [Enkel omvandlingsfunktion för datum i Arduino](https://www.circuitsathome.com/mcu/arduino/working-with-time-and-date-projects-converting-integer-data-to-string-date-/)