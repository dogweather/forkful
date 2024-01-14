---
title:                "Arduino: Skriver en streng med store bokstaver"
simple_title:         "Skriver en streng med store bokstaver"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng i et Arduino-program kan være nyttig for å gjøre teksten mer leselig for brukere. Dette kan være spesielt viktig når man skal vise informasjon på en LCD-skjerm eller seriell monitor.

## Hvordan

For å kapitalisere en streng i Arduino, kan du bruke funksjonen ```toUpperCase()```. Denne funksjonen tar inn en streng som parameter og returnerer en ny streng med alle bokstavene omgjort til store bokstaver.

```Arduino
String originalStreng = "dette er en test"; // Opprinnelig streng med små bokstaver
String kapitalisertStreng = originalStreng.toUpperCase(); // Bruker toUpperCase() for å kapitalisere strengen
Serial.println(kapitalisertStreng); // Skriver ut den kapitaliserte strengen på seriell monitor
```

Dette vil resultere i følgende utskrift: "DETTE ER EN TEST".

## Dypdykk

For å forstå hvordan funksjonen ```toUpperCase()``` fungerer, kan du se nærmere på kildekoden for Arduino-biblioteket. I kildekoden kan vi se at funksjonen først konverterer strengen til en char-array og deretter bruker en loop for å gå gjennom hvert tegn og konvertere det til en stor bokstav ved hjelp av ASCII-tabellen. Dette kan være nyttig å vite dersom du ønsker å tilpasse funksjonen eller lage din egen implementering.

## Se også

- Les mer om ```toUpperCase()``` i Arduino-dokumentasjonen: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/
- Utforsk ASCII-tabellen for å forstå hvordan bokstaver lagres som tall: https://www.asciitable.com/
- Finn flere Arduino-tips og triks på vår blogg: https://www.arduino.no/blogg/