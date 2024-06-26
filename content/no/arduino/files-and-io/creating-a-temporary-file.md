---
date: 2024-01-20 17:39:23.034853-07:00
description: "Hvordan: Opprettelse av midlertidige filer startet lenge f\xF8r Arduino,\
  \ i tidlige datamaskinsystemer for \xE5 h\xE5ndtere mellomlagring. P\xE5 Arduino\
  \ kan et SD-kort\u2026"
lastmod: '2024-04-05T22:50:55.084345-06:00'
model: gpt-4-1106-preview
summary: "Opprettelse av midlertidige filer startet lenge f\xF8r Arduino, i tidlige\
  \ datamaskinsystemer for \xE5 h\xE5ndtere mellomlagring."
title: Opprette en midlertidig fil
weight: 21
---

## Hvordan:
```Arduino
// Demonstrerer opprettelse av en midlertidig fil på et SD-kort med Arduino

#include <SPI.h>
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("Feil ved initialisering av SD-kortet!");
    return;
  }

  tempFile = SD.open("temp.txt", FILE_WRITE);

  if (tempFile) {
    tempFile.println("Midlertidig data...");

    Serial.println("Skrevet til temp.txt");
    
    tempFile.close(); // Lukker filen for å spare dataene
  } else {
    Serial.println("Feil ved opprettelse av filen!");
  }
}

void loop() {
  // Her kan annen kode kjøres - ingen tempfilkode nødvendig i loop
}
```
Sample output:
```
Skrevet til temp.txt
```

## Dypdykk
Opprettelse av midlertidige filer startet lenge før Arduino, i tidlige datamaskinsystemer for å håndtere mellomlagring. På Arduino kan et SD-kort brukes for dette formålet, som når man logger data over tid. Alternativt kan man bruke EEPROM, men skrive og slette-slitasje gir SD-kortet et fortrinn for midlertidig lagring.

Implementering krever et SD-kortmodul tilkoblet Arduino. Man må bruke SD-biblioteket for tilgang til filsystemet. Viktig å huske på: SD-kortet må være FAT16- eller FAT32-formatert. Feilhåndtering, som vist i koden, er kritisk for å sikre mot datatap.

## Se Også:
- SD bibliotekdokumentasjon: https://www.arduino.cc/en/Reference/SD
- Arduino EEPROM: https://www.arduino.cc/en/Reference/EEPROM
- Filbehandling i C++ (relevant for Arduino sin programmeringsmodell): http://www.cplusplus.com/reference/cstdio/fopen/
