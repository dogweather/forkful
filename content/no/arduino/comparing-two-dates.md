---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sammenligne to datoer er en prosess hvor man bestemmer hvilken dato som er tidligere eller senere, eller om de er identiske. Programmerere gjør dette for å utføre funksjoner som sortering av data, beregning av tidsintervaller, eller utløse hendelser basert på dato.

## Hvordan:

Her er et eksempel på hvordan man sammenligner to datoer i Arduino ved hjelp av TimeLib biblioteket:

```Arduino 
#include <TimeLib.h>
  
void setup() {
  Serial.begin(9600);
  
  tmElements_t tm1, tm2;

  tm1.Year = 2021 - 1970; tm1.Month = 7; tm1.Day = 21;
  tm2.Year = 2021 - 1970; tm2.Month = 8; tm2.Day = 10;
  
  time_t t1 = makeTime(tm1), t2 = makeTime(tm2);
  
  if(t1 > t2)
    Serial.println("Dato 1 er senere enn Dato 2");
  else if(t1 < t2)
    Serial.println("Dato 2 er senere enn Dato 1");
  else
    Serial.println("Datoene er identiske");
}
```

Dette vil skrive ut enten "Dato 1 er senere enn Dato 2", "Dato 2 er senere enn Dato 1" eller "Datoene er identiske" avhengig av datoene som er satt.

## Dypdykk:

Sammenligning av to datoer er en tidstestet programmeringsfunksjon, som har vært nødvendig siden de første kalender-applikasjonene. Det er imidlertid mange måter å utføre denne operasjonen på, avhengig av behovene til ditt spesifikke prosjekt.

For eksempel, i tillegg til TimeLib biblioteket, kan man også bruke DateTime biblioteket for lignende oppgaver. Valget av bibliotek kan avhenge av flere faktorer, som minnebruk og prosessorkrav.

I vår implementering bruker vi `makeTime()` funksjonen for å konvertere vår strukturdata til `time_t` format. Denne formen for tid er mer egnet for sammenligning.

## Se Også:

Følgende lenker inneholder mer informasjon om sammenligning av datoer og tid i Arduino:
1. [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
2. [DateTime Library For Arduino](https://www.arduino.cc/reference/en/libraries/rtc-datetime/)
3. [Arduino Time Manipulation](https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/troubleshooting)