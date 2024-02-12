---
title:                "Lese en tekstfil"
aliases:
- /no/arduino/reading-a-text-file.md
date:                  2024-01-20T17:53:51.432702-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil betyr å hente innhold fra en fil lagret på lagringsmediet, for eksempel et SD-kort, så du kan bruke dataene i programmet ditt. Programmerere gjør dette for å kunne arbeide med konfigurasjoner, lagre data eller logge informasjon uten å hardkode det direkte i Arduino-sketchen.

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
