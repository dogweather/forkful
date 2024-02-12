---
title:                "Een datum in de toekomst of het verleden berekenen"
aliases:
- /nl/arduino/calculating-a-date-in-the-future-or-past/
date:                  2024-01-28T21:55:35.669627-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum in de toekomst of het verleden berekenen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het berekenen van een datum in de toekomst of het verleden betekent het bepalen van een specifieke dag voor of na een gegeven datum. Programmeurs doen dit voor functies zoals het plannen van evenementen, herinneringen of het berekenen van deadlines.

## Hoe:

Arduino heeft geen ingebouwde datum- en tijdfuncties, maar je kunt de "TimeLib.h" bibliotheek gebruiken om datumberekeningen te verwerken. Zorg ervoor dat je de bibliotheek hebt geïnstalleerd voordat je de onderstaande voorbeelden gebruikt.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 3, 2023); // Stel tijd in op 25 maart 2023, 10:00:00
}

void loop() {
  // Bereken 10 dagen in de toekomst
  time_t futureTime = now() + 10 * SECS_PER_DAY;
  
  // Print toekomstige datum
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  // Bereken 10 dagen in het verleden
  time_t pastTime = now() - 10 * SECS_PER_DAY;
  
  // Print verleden datum
  Serial.print(day(pastTime));
  Serial.print("/");
  Serial.print(month(pastTime));
  Serial.print("/");
  Serial.println(year(pastTime));

  // Voorkom constant printen
  delay(10000);
}
```
Voorbeelduitvoer:
```
4/4/2023
15/3/2023
```

## Diepgaand

Voor RTC (real-time clock) modules en bibliotheek zoals TimeLib, was tijdbeheer op Arduino rudimentair en meestal handmatig geïmplementeerd. Er zijn verschillende manieren om toekomstige of verleden datums te berekenen, maar het gebruiken van een gespecialiseerde bibliotheek zoals TimeLib vereenvoudigt het proces aanzienlijk.

Alternatieven voor TimeLib zijn de meer uitgebreide "RTClib.h" voor gebruik met hardware RTC's, of de ingebouwde `millis()` functie voor kortere tijdsintervallen (met handmatig datumsbeheer). TimeLib behandelt schrikkeljaren en tijdzones en biedt hulpprogramma's voor eenvoudige datumsmanipulatie.

Let bij het berekenen van toekomstige of verleden datums op tijdzones en veranderingen van zomertijd als je werkt met real-time klokken of externe tijdsbronnen. Op Arduino, zonder een RTC of internetverbinding, stel je de tijd meestal handmatig in of via een extern signaal (zoals GPS of radiotijdsignalen).

## Zie ook

- Documentatie van de Time Library:
  https://www.arduino.cc/reference/en/libraries/time/
- RTClib, een populaire bibliotheek voor het werken met real-time klokken:
  https://github.com/adafruit/RTClib
- Arduino's millis() functie en het gebruik ervan:
  https://www.arduino.cc/reference/en/language/functions/time/millis/
