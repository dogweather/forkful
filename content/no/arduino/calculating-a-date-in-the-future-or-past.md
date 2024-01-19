---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Arduino: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er prosessen med å fastslå en bestemt dato basert på en gitt dato og et intervall. Programvareutviklere gjør dette for å håndtere oppgaver som planlegging, sporing av hendelser og tidshåndtering.

## Hvordan:
Her er en enkel måte å beregne en fremtidig dato ved hjelp av Arduino kode:

```Arduino
#include <TimeLib.h>

void setup() {
  setTime(12, 0, 0, 1, 1, 2020); // set time to 12:00:00am, Jan 1 2020
}

void loop() {
  time_t future_date = now() + (7 * SECS_PER_DAY); // calculate 7 days in future
  digitalClockDisplay(future_date);
  delay(1000);
}

void digitalClockDisplay(time_t t){
  Serial.print(hour(t));
  Serial.print(":");
  
  if (minute(t) < 10)
    Serial.print('0');
  Serial.print(minute(t));
  Serial.println();
}
```

Programmet vil da vise klokkeslettet for 7 dager frem i tid fra satt dato.

## Dyp Dykk
Historisk sett har beregning av fremtidige eller tidligere datoer vært en utfordring for programmerere, særlig på grunn av skuddår og ulikheter i månedslengder. 

Alternativer inkluderer bruk av robuste dato- og tidsbiblioteker som TimeLib på Arduino, som håndterer mange av disse detaljene for deg.

Ved implementering, vær oppmerksom på tidsbegrensninger og intervaller. For eksempel, Arduino's `millis()` funksjon tilbakestiller seg selv etter ca. 50 dager, noe som kan skape problemer i langvarige prosjekter.

## Se Også
- [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
- [Arduino's millis() Function](https://www.arduino.cc/reference/en/language/functions/time/millis/)