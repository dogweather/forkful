---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva en textfil innebär att lagra information i en läsbar form på ett lagringsmedium. Programmerare gör detta för att spara data, konfigurationer eller logga information från sensorer och processer.

## Hur gör man:
```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD-korts fel.");
    return;
  }
  myFile = SD.open("test.txt", FILE_WRITE); // Skapa en ny fil eller öppna befintlig
  if (myFile) {
    myFile.println("Hej Arduino!");
    myFile.close(); // Stäng filen efter användning
  } else {
    Serial.println("Fel vid öppning av filen.");
  }
}

void loop() {
  // Tom då vi bara skriver vid uppstart
}
```
Output i `test.txt`: "Hej Arduino!"

## Fördjupning:
I tidigt 2000-tal, med Arduino's framfart, blev det enklare för hobbyister och ingenjörer att spara data lokalt. Alternativ inkluderar EEPROM eller externa databaser men SD-kort är lättanvända och rymliga. Detaljer att notera är filsystemsformat (FAT16/FAT32), maxfilstorlek, och behov av `SD.h`-biblioteket.

## Se även:
- Arduino's SD biblioteksdokumentation: https://www.arduino.cc/en/Reference/SD
- FAT-filsystem: https://en.wikipedia.org/wiki/File_Allocation_Table
- EEPROM vs SD-kort för Arduino: https://www.arduino.cc/en/Tutorial/EEPROMReadWrite