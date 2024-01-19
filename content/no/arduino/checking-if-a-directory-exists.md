---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Arduino: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe eksisterer betyr ganske enkelt å finne ut om en spesifikk mappe finnes i systemminnet til Arduino. Vi gjør dette for å unngå feil som kan skje når vi prøver å åpne eller skrive til en mappe som ikke eksisterer.

## Hvordan:
Her er et eksempel på en funksjon som sjekker om en mappe eksisterer:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialisering mislykket!");
    return;
  }
  if (SD.exists("testfolder")) {
    Serial.println("Mappen eksisterer");
  } else {
    Serial.println("Mappen eksisterer ikke");
  }
}

void loop() {
}
```
Hvis mappen "testfolder" eksisterer på SD-kortet, vil du se "Mappen eksisterer" i serievinduet. Hvis ikke, vil du se "Mappen eksisterer ikke".

## Dyp Dykk
Når det gjelder historisk sammenheng, har denne evnen til å sjekke om en mappe eksisterer alltid vært en del av programmeringsspråk, selv før mikrokontrollere som Arduino.

Sjekking av mapper er en standardfunksjon i SD biblioteket for Arduino, men du kan også bruke andre biblioteker for å oppnå det samme, som SPIFFS biblioteket.

Når det gjelder implementasjonsdetaljer, fungerer `SD.exists()` funksjonen ved å sende en kommando til SD-kortet som ber det søke etter mappen. SD-kortet returnerer så et signal som indikerer om det fant mappen eller ikke.

## Se Også
For å lære mer om fil- og mappehåndtering med Arduino, sjekk følgende linker:
- Den offisielle Arduino hjemmesiden: https://www.arduino.cc/
- SD bibliotekets dokumentasjon: https://www.arduino.cc/en/Reference/SD
- Eksempelkode og mer detaljerte instruksjoner om bruk av SPIFFS biblioteket: https://randomnerdtutorials.com/esp8266-and-esp32-filesystems-spiffs-littlefs/