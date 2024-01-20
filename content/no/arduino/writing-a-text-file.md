---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil er prosessen med å lagre data i en fil på en datamaskin eller et oppkoblet enhet. Programmerere gjør dette for å lagre konfigurasjoner, logge data, eller bevare informasjon mellom økter.

## Slik gjør du:
```Arduino
#include <SD.h>

File myFile;
int chipSelect = 4; // Chip select pin for SD card module

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Kort initielt mislykket");
    return;
  }
  
  // Åpne filen. Merk: "DATA.TXT" vil bli opprettet om den ikke eksisterer.
  myFile = SD.open("DATA.TXT", FILE_WRITE);

  // Skrive tekst til filen
  if (myFile) {
    myFile.println("Hei, Arduino!");
    myFile.close(); // VIKTIG: Lukk filen for å spare endringene!
    Serial.println("Skriving vellykket");
  } else {
    Serial.println("Feil under åpning av filen");
  }
}

void loop() {
  // Ingenting her
}
```
Sample Output:
```
Skriving vellykket
```

## Dybde-dykking
Historisk så lagret Arduino data på EEPROM, men nå bruker man ofte et SD-kortmodul for større lagringsplass. Alternative måter å lagre data på inkluderer bruk av en nettbasert tjeneste eller andre typer hukommelse som FRAM. Implementasjonsdetaljer varierer etter lagringsmedium; med et SD-kort er det viktig å velge korrekt brikkevalg-pinne og initialisere kortet før skriving.

## Se også
- [Arduino SD-biblioteket](https://www.arduino.cc/en/reference/SD)
- [File IO på Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- [Arduino EEPROM-biblioteket](https://www.arduino.cc/en/Reference/EEPROM)