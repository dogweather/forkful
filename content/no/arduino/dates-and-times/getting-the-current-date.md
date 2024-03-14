---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.849050-07:00
description: "\xC5 f\xE5 tak i dagens dato i Arduino-prosjekter inneb\xE6rer \xE5\
  \ skaffe sanntidsinformasjon som kan v\xE6re avgj\xF8rende for logging, tidsstempling\
  \ eller planlegging av\u2026"
lastmod: '2024-03-13T22:44:41.068642-06:00'
model: gpt-4-0125-preview
summary: "\xC5 f\xE5 tak i dagens dato i Arduino-prosjekter inneb\xE6rer \xE5 skaffe\
  \ sanntidsinformasjon som kan v\xE6re avgj\xF8rende for logging, tidsstempling eller\
  \ planlegging av\u2026"
title: "F\xE5 den gjeldende datoen"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i dagens dato i Arduino-prosjekter innebærer å skaffe sanntidsinformasjon som kan være avgjørende for logging, tidsstempling eller planlegging av oppgaver. Programmerere trenger ofte denne funksjonaliteten for å forbedre funksjonalitet, sikre datarelevans og lette tidsfølsomme operasjoner i sine IoT- og innebygde prosjekter.

## Hvordan:
Arduino i seg selv har ikke en innebygd metode for å direkte hente dagens dato, ettersom den mangler et ekte tidsur (RTC). Dette kan imidlertid oppnås ved bruk av eksterne RTC-moduler som DS3231, og biblioteker som `RTClib`, utviklet av Adafruit, som gjør grensesnittet med disse modulene enkelt.

Først, sørg for at `RTClib`-biblioteket er installert i din Arduino IDE. Deretter, koble RTC-modulen din til Arduinoen i henhold til dens dokumentasjon.

Her er et enkelt eksempel for å komme i gang:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Fant ikke RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC mistet strøm, la oss sette tiden!");
    // Når tiden trenger å bli satt på en ny enhet eller etter et strømtap, kan du sette den her.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Dagens Dato: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Forsinkelse på 3 sekunder for å redusere seriell spam
}
```

Eksempel på utskrift (forutsatt at din RTC tidligere har blitt satt):

```
Dagens Dato: 2023/4/15
```

Denne koden initialiserer RTC-modulen og henter deretter og skriver ut dagens dato til Serial Monitor hvert 3. sekund. Husk, `rtc.adjust(...)`-linjen kan utkommenteres og modifiseres for å sette RTC's dato og tid opprinnelig eller etter at den har mistet strømmen.
