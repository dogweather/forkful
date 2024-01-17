---
title:                "Sammenligning av to datoer"
html_title:           "Arduino: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Comparing to datoer betyr å sjekke om de er like eller om en er større eller mindre enn den andre. Dette er viktig for programvareutviklere for å kunne sammenligne datoer og utføre forskjellige handlinger avhengig av resultatet.

# Hvordan:
Arduino har innebygd funksjonalitet for å sammenligne to datoer. Her er et eksempel på hvordan du kan gjøre det:

```Arduino
#include <RTClib.h>
RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  rtc.begin();
}

void loop() {
  DateTime dato1(2021, 05, 01, 11, 30, 00);
  DateTime dato2(2021, 05, 05, 15, 45, 00);

  // Sammenlign datoer og skriv ut resultatet
  if (dato1 < dato2) {
    Serial.println("Dato 1 er mindre enn dato 2");
  } else if (dato1 == dato2) {
    Serial.println("Dato 1 er lik dato 2");
  } else {
    Serial.println("Dato 1 er større enn dato 2");
  }

  // Vent i 5 sekunder
  delay(5000);
}
```

Output:

```
Dato 1 er mindre enn dato 2
```

# Dykk dypere:
- Historisk kontekst: Før moderne programmeringsspråk hadde innebygd funksjonalitet for å sammenligne datoer, måtte utviklere skrive egne algoritmer for å gjøre dette.
- Altenativer: Det finnes også biblioteker som kan brukes for å sammenligne datoer på en mer avansert måte.
- Implementeringsdetaljer: Datoene i eksempelet er satt manuelt, men du kan også hente datoer fra systemet for å sammenligne de med den nåværende datoen.

# Se også:
- [DateTime Library for Arduino](https://www.arduino.cc/reference/en/libraries/datetime/)
- [How to Compare Two Dates in Arduino](https://www.tutorialspoint.com/arduino/arduino_comparing_dates.htm)