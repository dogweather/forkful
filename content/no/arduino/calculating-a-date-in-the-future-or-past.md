---
title:    "Arduino: Beregning av dato i fremtiden eller fortiden"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å ville beregne en dato i fremtiden eller fortiden ved hjelp av en Arduino. Det kan være for å planlegge hendelser, lage alarmer eller bare for å teste ut dine programmeringsferdigheter.

## Hvordan 
For å beregne en dato i Arduino, må vi først importere biblioteket "Time.h". Deretter kan vi bruke funksjonen `makeTime()` for å opprette en variabel med ønsket dato og klokkeslett. La oss se på et eksempel der vi beregner en dato 30 dager fra i dag:

```Arduino

#include <Time.h>

void setup() {
  time_t now = makeTime(0, 0, 0, day() + 30, month(), year());
  Serial.print("Datoen om 30 dager vil være: ");
  Serial.print(day(now));
  Serial.print("/");
  Serial.print(month(now));
  Serial.print("/");
  Serial.println(year(now));
}

void loop() {
  // Ingenting som skjer i løkken
}
```

Koden over vil skrive ut datoen 30 dager fra nå i serieporten. Du kan endre "30" til et annet tall for å beregne en annen dato i fremtiden eller fortiden. 

## Deep Dive
For å beregne en dato i fremtiden eller fortiden, bruker Arduino biblioteket "Time.h" som gir oss enkelt tilgang til nåværende dato og klokkeslett. Funksjonen `makeTime()` tar inn år, måned, dag, time, minutt og sekunder som parametere og returnerer verdien av tiden i sekunder siden midnatt 1. januar 1970. Dette er kjent som Epoch-tiden og brukes ofte i programmering for å håndtere dato og tid.

Det er også mulig å bruke funksjonen `breakTime()` for å bryte ned Epoch-tiden til en enkel lesbar form, som dag, måned, år, osv. Dette kan være nyttig hvis du ønsker å utføre beregninger på en spesifikk del av datoen.

## Se Også
- [Bibloteket "Time.h" i Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/TimeManipulation)
- [Epoch-tiden](https://no.wikipedia.org/wiki/Epoch-tiden)
- [Beregning av dato og klokkeslett i Arduino](https://www.instructables.com/id/Calculating-Date-and-Time-in-Arduino/)