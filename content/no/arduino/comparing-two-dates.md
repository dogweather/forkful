---
title:                "Sammenligner to datoer"
html_title:           "Arduino: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor sammenligne to datoer? Det kan være nyttig å vite når en hendelse skjedde i forhold til en annen, for eksempel når et sensoravlesning ble gjort i forhold til et tidspunkt en alarm ble utløst.

## Slik gjør du det
For å kunne sammenligne to datoer trenger vi å konvertere dem til tall som Arduino kan forstå. Dette gjøres ved hjelp av funksjonen `time.mktime()` som tar et `struct_time` objekt som argument. Deretter kan vi sammenligne de to datoene ved å bruke vanlige matematiske operatorer som `<, >, ==`.

```Arduino
#include <TimeLib.h>  // importerer Time Library
#include <Time.h>  // importerer Time.h biblioteket

// Opprett to datoer ved hjelp av struct_time objekter
struct_time dato1 = { 30, 30, 8, 26, 4, 2021 }; // 26. april 2021
struct_time dato2 = { 0, 0, 12, 24, 12, 2020 }; // 24. desember 2020

// Konverterer datoene til tall ved hjelp av time.mktime()
time_t tid1 = time.mktime(&dato1);
time_t tid2 = time.mktime(&dato2);

// Sammenligner datoene ved å bruke matematiske operatorer
if (tid1 < tid2) {
  Serial.println("Dato 1 er tidligere enn dato 2");
}
else if (tid1 > tid2) {
  Serial.println("Dato 2 er tidligere enn dato 1");
}
else { // Hvis de to datoene er like
  Serial.println("Dato 1 og dato 2 er like");
}

// Output: Dato 2 er tidligere enn dato 1
```

## Dykk dypere
For de som er interessert i å lære mer om datohåndtering i Arduino, er det verdt å se nærmere på "Time Library" og "Time.h" bibliotekene. Time Library gir flere funksjoner for å håndtere og manipulere datoer, mens Time.h biblioteket gir tilgang til systemdatoen og tidsfunksjoner for Arduino.

## Se også
- [Time Library referanse](https://playground.arduino.cc/Code/Time)
- [Time.h referanse](https://www.arduino.cc/en/Reference/Time)
- [Offisiell Arduino nettside](https://www.arduino.cc/)