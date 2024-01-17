---
title:                "Lage en midlertidig fil"
html_title:           "Arduino: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En midlertidig fil er en midlertidig lagret fil som brukes av programmerere for å lagre data eller informasjon. Dette gjøres for å unngå å overskrive eller miste viktig informasjon mens programmet kjører.

## Slik gjør du det:
```Arduino
#include <SPI.h>
#include <SD.h>

File tempFile; // Oppretter en variabel for den midlertidige filen

void setup() {
  SD.begin(10); // Initialiserer SD-kortet
  tempFile = SD.open("TEMP.txt", FILE_WRITE); // Oppretter og åpner den midlertidige filen

  if (tempFile) { // Sjekker om filen ble opprettet og åpnet riktig
    tempFile.println("Dette er en midlertidig fil."); // Skriver informasjon til filen
    tempFile.close(); // Lukker filen
  }
}

void loop() {
  // Kan bruke den midlertidige filen til å lagre data eller informasjon
}
```

## Dykk dypere:
Opprettelsen av midlertidige filer har vært en vanlig praksis blant programmerere i lang tid. Dette ble gjort for å sikre at viktig data ikke gikk tapt under programkjøring og for å unngå å overskrive andre filer som allerede eksisterer.

Som en alternativ metode, kan programmerere også bruke variabler til å lagre midlertidig informasjon. Dette kan være et enklere alternativ i mindre og enklere programmer. Imidlertid kan det å bruke midlertidige filer være mer effektivt for å lagre større mengder data.

Implementeringen av midlertidige filer kan variere avhengig av programmeringsspråk og operativsystem. I Arduino bruker vi funksjonen ```SD.open()``` for å opprette og åpne filen.

## Se også:
- [SD.h bibliotekdokumentasjon](https://www.arduino.cc/en/Reference/SD)
- [Fildeling og lagring i Arduino](https://www.arduino.cc/en/Tutorial/FileReadWrite)