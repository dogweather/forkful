---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng betyr å endre datoen fra sitt opprinnelige format til et tekstformat. Dette gjør det lettere for programmerere å vise datoen på en lesbar måte for mennesker, eller å samle inn og jobbe med datoen i visse applikasjoner.

## Hvordan:

Her er en enkel Arduino-kode for å konvertere en dato til en streng:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(22,30,0,1,1,2019); // setter tid og dato
}

void loop() {
  time_t t = now(); // henter nåværende tid

  String dateStr = String(day(t)) + "." + String(month(t)) + "." + String(year(t)); // konverterer dato til streng
  Serial.println(dateStr); // skriver ut datoen som en streng

  delay(1000);
}
```

Eksempel på output:
```
1.1.2019
```

## Dyp Dykk

Historisk kontekst: Konvertering av en dato til en streng er en klassisk programmeringsoppgave. Den første inkluderingen av dette i programmeringsspråk kom med C-språkets strftime-funksjon på 1970-tallet. 

Alternativer: I Arduino og C++ kan du også bruke sprintf og strftime funksjoner for konvertering. Men for Arduino, som er ment for å være så enkel som mulig, er det å bruke en String objekt og pluss operatør en foretrukket og enklere måte.

Implementeringsdetaljer: Arduino lagrer innstillingene for tid og dato i RTC (Real Time Clock) og bruker TimeLib biblioteket for å hente nåværende tid. Du bruker så noen enkle funksjoner (day(), month(), year()) for å konvertere disse til deler av en dato, og en String objekt for å opprette selve datostrengen.

## Se Også

TimeLib biblioteket: 
https://www.arduinolibraries.info/libraries/time

Arduino String referanse: 
https://www.arduino.cc/reference/en/language/variables/data-types/string/

C++ strftime funksjon: 
http://www.cplusplus.com/reference/ctime/strftime/