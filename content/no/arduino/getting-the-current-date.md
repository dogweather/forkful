---
title:                "Få gjeldende dato"
html_title:           "Arduino: Få gjeldende dato"
simple_title:         "Få gjeldende dato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å få tak i den nåværende datoen er en vanlig oppgave for programmører. Dette er nyttig for å utføre handlinger basert på dags- eller tidspunktet, for eksempel å automatisk slå på lysene i huset hver kveld. 

# Hvordan:
Bruk ```Arduino.now()``` funksjonen for å få den nåværende datoen og tiden. Her er et eksempel på å skrive datoen til seriell monitor:

```Arduino
#include <RTClib.h>  // inkluderer biblioteket for sanntidsklokke
RTC_DS1307 rtc; // Oppretter et RTC-objekt

void setup ()
{
  Serial.begin(9600); // Starter seriell kommunikasjon
  rtc.begin(); // Starter sanntidsklokken
}

void loop ()
{
  DateTime now = rtc.now(); // Får den nåværende datoen og tiden
  Serial.print("Nåværende dato: ");
  Serial.print(now.day(), DEC); // Skriver ut dagen som en desimal
  Serial.print('/');
  Serial.print(now.month(), DEC); // Skriver ut måneden som en desimal
  Serial.print('/');
  Serial.print(now.year(), DEC); // Skriver ut året som en desimal
  Serial.print("  Nåværende tid: ");
  Serial.print(now.hour(), DEC); // Skriver ut timen som en desimal
  Serial.print(':');
  Serial.print(now.minute(), DEC); // Skriver ut minuttet som en desimal
  Serial.print(':');
  Serial.println(now.second(), DEC); // Skriver ut sekundet som en desimal
  delay(1000); // Venter et sekund før den gjentar loopen
}
```

Output: ``` 
Nåværende dato: 24/11/2020  Nåværende tid: 21:48:00
```

# Dypdykk:
Før i tiden var det vanlig å bruke en separat komponent kalt en sanntidsklokke for å kunne få tak i den nåværende datoen og tiden. Med Arduino og RTClib-biblioteket kan du nå enkelt bruke en integrert sanntidsklokke som finnes på mange Arduino-brett. En alternativ måte å få den nåværende datoen på er å koble til internett og bruke en internett-tidsklokke som NTP (Network Time Protocol).

# Se også:
[Offisiell Arduino nåværende tid og dato dokumentasjon](https://www.arduino.cc/en/Tutorial/BuiltInExamples/timedate)<br>
[RTClib biblioteket dokumentasjon](https://github.com/adafruit/RTClib)<br>
[En grundig guide om å koble til internett og få tak i den nåværende datoen og tiden](https://www.best-microcontroller-projects.com/arduino-time-library.html)