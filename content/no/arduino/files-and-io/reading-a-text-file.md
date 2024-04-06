---
date: 2024-01-20 17:53:51.432702-07:00
description: "Hvordan gj\xF8re det: \xC5 lese tekstfiler er en grunnleggende funksjon\
  \ som har v\xE6rt en del av programmering siden begynnelsen. For Arduino er microSD-kort\
  \ via\u2026"
lastmod: '2024-04-05T22:50:55.082136-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese tekstfiler er en grunnleggende funksjon som har v\xE6rt en del\
  \ av programmering siden begynnelsen."
title: Lese en tekstfil
weight: 22
---

## Hvordan gjøre det:
```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // Vent på at seriell tilkobling skal etablere.
  }

  if (!SD.begin(4)) {
    Serial.println("Initialisering av SD feilet!");
    return;
  }

  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Feil ved åpning av filen!");
  }
}

void loop() {
  // Ingenting å gjøre her
}
```
*Sample output:*  
```
Hei, dette er en testfil.
Dette er andre linje i filen.
```

## Dypdykk
Å lese tekstfiler er en grunnleggende funksjon som har vært en del av programmering siden begynnelsen. For Arduino er microSD-kort via SPI-kommunikasjon et vanlig alternativ. Du kan også bruke EEPROM, men det har mer begrenset plass. Viktige detaljer inkluderer riktig initialisering av SD-modulen og håndtering av åpnings- og lukkingsprosedyrer for filene for å unngå korrupte data og minnelekasje.

Historisk sett er disse prinsippene sentrale i mange programmeringsspråk og utviklerverktøy, selv om syntaks og metoder varierer. Alternativt kan data strømmes over nettverk eller gjennom seriel kommunikasjon, men lesing fra SD-kort er kompakt og pålitelig for mange Arduino-prosjekter.

## Se også
- [Arduino's SD bibliotek](https://www.arduino.cc/en/Reference/SD)
- [SPI kommunikasjon med Arduino](https://www.arduino.cc/en/reference/SPI)
