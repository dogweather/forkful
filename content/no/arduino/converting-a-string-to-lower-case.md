---
title:                "Arduino: Fra string til små bokstaver"
simple_title:         "Fra string til små bokstaver"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å konvertere en streng til små bokstaver i Arduino-programmering. For eksempel kan du ønske å sammenligne to strenger for å se om de er like, uavhengig av om de er skrevet med små eller store bokstaver. Eller kanskje du ønsker å formatere en streng på en bestemt måte før du viser den på en skjerm. Uansett årsak, er konvertering av en streng til små bokstaver en grunnleggende funksjon som kan være nyttig i en rekke forskjellige programmer.

## Hvordan

Å konvertere en streng til små bokstaver i Arduino er en ganske enkel prosess. Du trenger bare å bruke den innebygde funksjonen "toLowerCase()", som tar en streng som argument og returnerer en ny streng med alle bokstaver i små bokstaver. La oss se på et eksempel:

```Arduino
String start = "HEI VERDEN";

// Konverter strengen til små bokstaver
String ny_streng = start.toLowerCase();

// Skriv ut den nye strengen
Serial.println(ny_streng);
```

Dette vil resultere i at "HEI VERDEN" blir skrevet ut som "hei verden". Som du kan se, er det enkelt å konvertere en streng til små bokstaver ved å bruke denne funksjonen. Dette kan gjøres med alle typer strenger, uavhengig av lengde eller antall bokstaver.

## Dypdykk

For de som er interessert i den dypere tekniske informasjonen om konvertering av strenger til små bokstaver, kan det være nyttig å vite at denne funksjonen ikke endrer den opprinnelige strengen, men snarere lager en ny streng med små bokstaver. Dette betyr at hvis du vil beholde den originale strengen med store bokstaver, bør du lagre den i en annen variabel før du konverterer den.

Det er også viktig å merke seg at denne funksjonen bare kan brukes på ASCII-karakterer. Dette inkluderer de fleste engelske alfabetet, tall og vanlige symboler. Hvis du ønsker å konvertere en streng med ikke-ASCII-karakterer til små bokstaver, kan du bruke en tilpasset funksjon som du finner en lang rekke av på nettet.

## Se også

- [Referanseside for toLowerCase() funksjonen i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Videoveiledning om å konvertere strenger til små bokstaver i Arduino](https://www.youtube.com/watch?v=A-ptiLjNIpM)
- [Eksempelprosjekt som bruker konvertering av strenger i et LCD-skjerm-display](https://create.arduino.cc/projecthub/Arduino_Genuino/lcd-display-afc76e)