---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk er en metode for å matche mønstre i tekst. Dette er gunstig for programmerere fordi det gir evnen til å finne, endre og validere tekst i større datamengder automatisk.

## Hvordan:

Her er et enkelt Arduino-eksempel som bruker regulære uttrykk for å finne alle alfabetbokstavene i et gitt datasett:

```Arduino
#include <Regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial);
  
  Regex r("[a-zA-Z]+");

  if (r.match("Hei, jeg er en Arduino!")) {
    Serial.println(r.count());  // vil skrive ut '4'
  }
}

void loop() {}
```

Dette vil resultere i '4' på serieovervåkningen, noe som indikerer at det er fire ord i teksten vår.

## Dyp dykk

Regulære uttrykk ble først introdusert i 1950- og 60-årene for automatisk tekstbehandling. Alternativt kan uttrykksmatching utføres ved hjelp av for-løkker og if-setninger, men dette kan bli veldig rotete og uoversiktlig. Når det gjelder implementeringsdetaljer for Arduino, bruker vi Regex-biblioteket som støtter mest brukte regulære uttrykksfunksjoner, inkludert matching og gjenkjenningsnummer.

## Se også

For mer detaljert informasjon og avanserte konsepter relatert til regulære uttrykk i Arduino, kan du sjekke følgende ressurser:

1. Det offisielle Arduino Regex-biblioteket: https://github.com/nickgammon/Regexp
2. En grunnleggende innføring til regulære uttrykk: https://www.learn-c-regex.com