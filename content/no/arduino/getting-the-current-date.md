---
title:    "Arduino: Hente dagens dato"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å få tak i den nåværende datoen er en viktig del av mange programmeringsprosjekter. Det er spesielt nyttig for å lage tidsbaserte funksjoner og for å holde rede på når en bestemt hendelse skjedde.

## Hvordan
For å få tak i den nåværende datoen i Arduino, kan du bruke funksjonene som er tilgjengelige i den innebygde tidsbiblioteket. Her er et eksempel på hvordan du kan skrive ut den nåværende datoen på en serie monitor:
```Arduino
#include <TimeLib.h>
void setup() {
  Serial.begin(9600);
  setTime(13, 30, 45, 1, 5, 2021);
}
void loop() {
  tmElements_t now;
  breakTime(now, now);
  Serial.print(now.Day);
  Serial.print("/");
  Serial.print(now.Month);
  Serial.print("/");
  Serial.print(now.Year);
}
```
Dette vil skrive ut datoen som "5/1/2021" på serie monitoren. Det er også mange andre funksjoner som lar deg få tak i den nåværende tiden, inkludert å få tak i tidsstempler i millisekunder og konvertere dem til en mer lesbar format.

## Deep Dive
En ting å huske på når du arbeider med å få tak i den nåværende datoen er at Arduino ikke har en innebygd klokke eller kalender. Derfor må du manuelt sette opp dato og tid ved å bruke funksjonen `setTime()` i setup-seksjonen av programmet ditt. Du kan også bruke en ekstern Real Time Clock (RTC) modul for å få mer nøyaktig og pålitelig tidsinformasjon.

En annen viktig ting å huske på er å konvertere tiden du får fra Arduino til det aktuelle tidssonen og formatet du trenger. Dette kan gjøres ved å bruke funksjonene som er tilgjengelige i tidsbiblioteket, eller du kan manuelt beregne og justere for eventuelle tidssoner.

## Se også
- TimeLib dokumentasjon: https://github.com/PaulStoffregen/Time
- De forskjellige funksjonene for å arbeide med tid i Arduino: https://www.arduino.cc/en/Reference/Time
- RTC modul tutorial: https://www.arduino.cc/en/Tutorial/RTClib