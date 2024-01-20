---
title:                "Å skrive en tekstfil"
html_title:           "Arduino: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil betyr å lage en fil med tekst som kan leses og endres av et dataprogram. Programmerere gjør dette for å lagre informasjon som kan brukes senere, som for eksempel sensoravlesninger eller brukerinnstillinger.

## Slik gjør du:
For å skrive en tekstfil på Arduino, bruker du funksjonen ```File.print()``` eller ```File.println()``` etterfulgt av teksten du vil skrive. Du må også inkludere ```#include <SPI.h>``` i begynnelsen av programmet ditt for å kunne bruke denne funksjonen. Eksempelet nedenfor viser hvordan du skriver teksten "Hei verden!" til en fil kalt "test.txt".

Arduino-kode:
```
#include <SPI.h>

void setup() {
  SPI.begin();
  File dataFile = SD.open("test.txt", FILE_WRITE);
  dataFile.print("Hei verden!");
  dataFile.close();
}

void loop() {

}
```
Etter å ha lastet opp koden til Arduinoen din og åpnet "test.txt" på datamaskinen din, bør du se teksten "Hei verden!" inne i filen. Merk at "FILE_WRITE" -parameteret i ```SD.open()``` -funksjonen betyr at filen skal åpnes for skriving.

## Dykk dypere:
I historisk sammenheng ble tekstfiloppsettet utviklet for å lagre dokumenter på datamaskiner på 1960-tallet. Det finnes også alternative måter å lagre informasjon på, som for eksempel å bruke EEPROM eller EEPROM-emuleringsbiblioteker. Det kan også være nyttig å vite at "SPI" i ```#include <SPI.h>``` står for "Serial Peripheral Interface" og brukes til å kommunisere med SD-kortet.

## Se også:
- [Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)