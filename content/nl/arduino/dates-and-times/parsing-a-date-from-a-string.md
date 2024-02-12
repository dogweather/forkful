---
title:                "Een datum uit een string parsen"
aliases: - /nl/arduino/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:03:52.857588-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een datum uit een string parsen betekent het extraheren van de datumgegevens zoals dag, maand en jaar, en deze converteren naar een formaat dat een computer kan begrijpen. Programmeurs doen dit omdat de datum- en tijdgegevens vaak in een gestructureerde vorm nodig zijn om bewerkingen uit te voeren zoals vergelijkingen, berekeningen, of opslaan in een database.

## Hoe:

Laten we een string omzetten in een datum:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Kon RTC niet vinden");
    while (1);
  }
  
  // Laten we aannemen dat de datumstring in het formaat "DD/MM/JJJJ" is
  String dateString = "24/12/2023"; 
  
  int dag = dateString.substring(0, 2).toInt();
  int maand = dateString.substring(3, 5).toInt();
  int jaar = dateString.substring(6).toInt();
  
  rtc.adjust(DateTime(jaar, maand, dag));
  
  Serial.print("Datum ingesteld op: ");
  Serial.print(dag);
  Serial.print("/");
  Serial.print(maand);
  Serial.print("/");
  Serial.println(jaar);
}

void loop() {
  // Hier doen we niets
}
```

Voorbeelduitvoer:
```
Datum ingesteld op: 24/12/2023
```

## Diepere Duik

Datums parsen is een veelvoorkomende taak sinds de vroege dagen van het programmeren. Historisch gezien was het hanteren van datums platformspecifiek en foutgevoelig. De Arduino, met zijn vele bibliotheken zoals RTClib, vereenvoudigt dit proces aanzienlijk.

Alternatieven voor RTClib voor het parsen van datums omvatten het gebruik van ingebouwde functies of het schrijven van aangepaste code om datumstrings te valideren en te converteren. Implementatiedetails zoals controleren op schrikkeljaren of omgaan met verschillende datumformaten kunnen het parsen complex maken. Zorgen dat invoerstrings in verwachte formaten zijn en het controleren van geparsede waarden op fouten zijn cruciaal om storingen te voorkomen.

## Zie ook

- RTClib op GitHub: https://github.com/adafruit/RTClib
- Arduino Time Library: https://www.arduino.cc/reference/en/libraries/time/
- Arduino DateTime Class referentie: https://github.com/adafruit/RTClib/blob/master/DateTime.h
