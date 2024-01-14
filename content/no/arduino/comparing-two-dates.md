---
title:    "Arduino: Sammenligning av to datoer"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer kan være nyttig i mange forskjellige Arduino-prosjekter. Dette kan hjelpe deg med å holde styr på tidssensitive hendelser, som for eksempel når et batteri ble byttet eller når et sensor ble aktivert.

## Slik gjør du det
Det er flere måter å sammenligne to datoer på, avhengig av hva du ønsker å oppnå. Her er et eksempel på hvordan du kan sammenligne årets dato (den nåværende datoen) med en bestemt dato:

```Arduino
#include <Time.h> // inkluderer Time biblioteket

void setup() {
  Serial.begin(9600); // Initiere seriell kommunikasjon med en baud rate på 9600
  setTime(14, 0, 0, 1, 1, 2020); // Setter klokken til 14:00:00 1. januar 2020
}

void loop() {
  time_t now = now(); // Lagrer den nåværende datoen og tiden
  time_t compareDate = makeTime(0, 0, 0, 1, 1, year()); // Oppretter en time_t verdi for årets dato

  if (now == compareDate) { // Sammenligner nåværende dato med årets dato
    Serial.println("Godt nyttår!"); // Skriver ut en beskjed hvis de er like
  }
}
```

Denne koden vil skrive ut "Godt nyttår!" når datoen er 1. januar 2020 og klokken er 14:00.

## Dypt dykk
Å sammenligne to datoer kan også gjøres ved å bruke `time_t` variabler og funksjoner som `now()` og `makeTime()`. Du kan også bruke datofunksjonene som er tilgjengelige i Time biblioteket, som for eksempel `day()`, `month()` og `year()`.

Hvis du ønsker å sammenligne datoer i en løkke, kan du bruke et `for`-løkke som går gjennom hver dag og sjekker om de er like. Du kan også bruke `break`-kommandoen for å avslutte løkken når du finner en match.

Å kunne sammenligne datoer kan også være nyttig når du skal implementere en alarm eller tidsbasserte handlinger i prosjektet ditt.

## Se også
- [Time biblioteket dokumentasjon](https://playground.arduino.cc/code/time/)
- [Tutorial: Håndtering av dato og klokkeslett på Arduino](https://www.arduino.cc/en/tutorial/time/)
- [Eksempler på dato og klokkeslett bruk med Arduino](https://www.arduino.cc/en/Tutorial/DateStrings)