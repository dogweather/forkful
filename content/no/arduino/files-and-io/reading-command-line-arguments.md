---
date: 2024-01-20 17:55:29.609622-07:00
description: "Slik gj\xF8r du: Arduino-plattformen er tradisjonelt ikke linjeorientert\
  \ \u2013 den har ikke et vanlig kommandolinjegrensesnitt i likhet med enkelte\u2026"
lastmod: '2024-03-13T22:44:41.073481-06:00'
model: gpt-4-1106-preview
summary: "Arduino-plattformen er tradisjonelt ikke linjeorientert \u2013 den har ikke\
  \ et vanlig kommandolinjegrensesnitt i likhet med enkelte operativsystemer."
title: Lese kommandolinjeargumenter
weight: 23
---

## Slik gjør du:
Arduino-plattformen er tradisjonelt ikke linjeorientert – den har ikke et vanlig kommandolinjegrensesnitt i likhet med enkelte operativsystemer. Derfor simulerer vi argumenter via Serial-kommunikasjon.

```Arduino
void setup() {
  Serial.begin(9600); // Starter seriel kommunikasjon med 9600 baud
  while (!Serial) {
    ; // Vent til Serial-porten åpner
  }
}

void loop() {
  if (Serial.available() > 0) {
    // Les neste kommando
    String command = Serial.readStringUntil('\n');
    
    // Gjør noe basert på kommandoen
    if (command == "blink") {
      // Blink LED-kode 
    } else if (command == "status") {
      Serial.println("Systemet kjører.");
    }
  }
}
```

Eksempel på output:
```
Systemet kjører.
```

## Dykk dypere
Tradisjonell kommandolinjelesing, som i POSIX systemer, bruker `argc` og `argv` for å håndtere argumenter; Arduino har ikke denne mekanismen. Men siden Arduino kan koble til en datamaskin via USB, bruker vi Serial-kommunikasjon som et middel for å sende inn argumentlignende data. Historisk har Arduinos opprinnelige formål vært å kjøre forhåndsprogrammert kode hvor argumenter var lite relevant, derfor det indirekte alternativet gjennom Serial. I implementering av Serial-kommunikasjon, husk at Arduino må vente til et Serial-vindu er tilgjengelig før den kan sende data tilbake til datamaskinen.

## Se også
- Arduino Serial Communication:
  - https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Seriell kommunikasjon tutorial:
  - https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialEvent
