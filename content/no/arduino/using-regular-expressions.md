---
title:                "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er interessert i å lage mer avanserte og effektive programmer på Arduino, kan det være lurt å lære mer om regulære uttrykk. Dette er et kraftig verktøy som kan hjelpe deg med å søke og manipulere tekst på en mer fleksibel måte.

## Hvordan
Først må du importere biblioteket "RegExp" ved å legge til følgende linje i koden din:
```Arduino
#include <RegExp.h>
```

Deretter kan du bruke funksjonene i dette biblioteket for å definere og søke etter regulære uttrykk. For eksempel, hvis du ønsker å finne alle tall i en streng, kan du bruke følgende kode:
```Arduino
RegExp regex("\\d+");
while(regex.find("Dette er tall 123 og 456")) {
  Serial.println(regex.match());
}
```
Dette vil skrive ut 123 og 456 i Serial Monitor.

## Dypdykk
Regulære uttrykk følger et bestemt mønster for søk og manipulering av strenger. Det er forskjellige symboler og uttrykk du kan bruke for å definere mønsteret du ønsker å søke etter. For eksempel kan du bruke uttrykket " \d+" for å finne alle tall i en streng, der "\d" representerer et hvilket som helst tall og "+" betyr at det kan være flere tall etter hverandre.

Du kan også bruke regulære uttrykk til å erstatte deler av en streng, eller for å gjøre mer avanserte søk som tar hensyn til store og små bokstaver.

Det er viktig å merke seg at regulære uttrykk kan være komplekse og ta litt tid å mestre, men det er absolutt verdt innsatsen for å forbedre programmene dine.

## Se også
- [Arduino på norsk](https://www.arduino.cc/reference/no/)
- [RegExp biblioteket](https://github.com/kroimon/Arduino-RegExp)
- [Regulære Uttrykk i Arduino](http://www.nbl.fi/~nbl8991/prog/Arduino-Regexp/RegexInArduino.html)