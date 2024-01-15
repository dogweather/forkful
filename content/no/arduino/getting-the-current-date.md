---
title:                "Å få nåværende dato"
html_title:           "Arduino: Å få nåværende dato"
simple_title:         "Å få nåværende dato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Å kunne få nåværende dato er en nyttig funksjon i mange Arduino-prosjekter. Det kan hjelpe deg med å organisere data, logger, og planlegge hendelser i fremtiden.

## Hvordan
For å få nåværende dato på Arduino, må du først inkludere biblioteket `RTClib`. Dette biblioteket lar deg kommunisere med Real Time Clock (RTC) moduler som kan holde styr på dato og tid.

```Arduino
#include <RTClib.h>
```

Deretter må du opprette et objekt fra biblioteket, og koble det til riktig RTC modul. Dette kan variere avhengig av hvilken modell av RTC modul du bruker. Her er et eksempel med DS1307 modulen:

```Arduino
RTC_DS1307 rtc;
```

Nå kan du initialisere objektet og starte kommunikasjonen med RTC modulen:

```Arduino
void setup() {
  rtc.begin();
}
```

Til slutt, for å få nåværende dato, må du bruke funksjonen `now()` og lagre datoen i en variabel. Du kan også formatere datoen ved hjelp av flere innebygde funksjoner. Se eksempelet nedenfor:

```Arduino
DateTime now = rtc.now();
Serial.print(now.year()); // skriver ut nåværende år
Serial.print("/"); 
Serial.print(now.month()); // skriver ut nåværende måned
Serial.print("/"); 
Serial.print(now.day()); // skriver ut nåværende dag
```

## Deep Dive
Det er verdt å merke seg at RTC moduler ikke er like nøyaktige som andre tidssynkroniseringsteknologier. Derfor kan det være lurt å kalle funksjonen `now()` på nytt jevnlig for å få en mer presis dato. Du kan også justere tiden manuelt ved å bruke funksjonen `adjust()`.

En annen ting å være oppmerksom på er at RTC modulen kan slite med å holde på riktig dato og tid hvis den ikke blir tilstrekkelig forsynt med strøm. Husk derfor å bruke en batteribackup hvis det er nødvendig for ditt prosjekt.

## Se også
- [RTClib bibliotek dokumentasjon] (https://github.com/adafruit/RTClib)
- [Arduino time library guide] (https://www.arduino.cc/reference/en/libraries/time/)
- [DS1307 RTC modul guide] (https://howtomechatronics.com/tutorials/arduino/arduino-ds1307-rtc-clock-tutorial/)