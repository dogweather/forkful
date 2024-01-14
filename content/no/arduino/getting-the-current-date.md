---
title:                "Arduino: Å få nåværende dato"
simple_title:         "Å få nåværende dato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen kan være nyttig for en rekke ulike prosjekter som involverer tid og kalendere. Enten det er å lage en klokke eller en alarm, overvåke vanning av planter eller bare for å vise dagens dato på en skjerm, er det viktig å kunne få tilgang til riktig dato informasjon.

## Hvordan

Det er flere måter å få den nåværende datoen på i Arduino-programmering, men den enkleste metoden er å bruke en innebygd funksjon kalt "now()" fra biblioteket "Time.h". Her er et eksempel på hvordan du kan bruke denne funksjonen for å få datoen i et bestemt format:

```Arduino
#include <Time.h>

// Oppsettfunksjon, kjører en gang i begynnelsen
void setup() {
    // Initialiser serielkommunikasjon med en baudrate på 9600
    Serial.begin(9600);

    // Sett opp datalagring for å lagre datoen
    Time.setDate(DATAYEAR, DATAMONTH, DATADAY);
}

// Hovedløkkefunksjon, kjører gjentatte ganger etter setup
void loop() {
    // Bruk now()-funksjonen for å få dagens dato som et Time-objekt
    Time now = now();

    // Bruk Time-objektet til å få riktig format for datoen
    // Dette eksemplet vil skrive ut datoen i formatet DD.MM.YYYY
    Serial.println(String(now.day()) + "." + String(now.month()) + "." + String(now.year()));

    // Vent i ett sekund før du gjør det igjen
    delay(1000);
}
```

## Dypdykk

Ved å bruke biblioteket "Time.h" kan du få tilgang til ulike funksjoner for å arbeide med tid og datoer. For eksempel kan du bruke "day()", "month()" og "year()" funksjonene for å få tilgang til en bestemt del av datoen. Du kan også bruke "dayOfWeek()" funksjonen for å få informasjon om hvilken ukedag datoen faller på.

En annen måte å få datoen på er å bruke en ekstern realtids klokke modul. Disse modulene har innebygd krets som lar deg få tilgang til den nåværende datoen og klokkeslettet. I så fall må du bruke et annet bibliotek for å kommunisere med modulen, som for eksempel "RTClib.h".

## Se også

- [Time.h biblioteket dokumentasjon](https://playground.arduino.cc/code/time/)
- [RTClib.h biblioteket dokumentasjon](https://github.com/adafruit/RTClib)
- [Tutorial om hvordan du bruker en extern realtids klokke modul med Arduino](https://learn.adafruit.com/adafruit-arduino-lesson-12-lcd-displays-part-2/using-a-real-time-clock)