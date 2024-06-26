---
date: 2024-01-20 17:53:39.884379-07:00
description: "S\xE5 h\xE4r g\xF6r du: Arduino kan hantera filer p\xE5 ett SD-kort\
  \ med SD-biblioteket. H\xE4r \xE4r hur man \xF6ppnar och l\xE4ser en textfil."
lastmod: '2024-03-13T22:44:38.185282-06:00'
model: gpt-4-1106-preview
summary: "Arduino kan hantera filer p\xE5 ett SD-kort med SD-biblioteket."
title: "L\xE4sa en textfil"
weight: 22
---

## Så här gör du:
Arduino kan hantera filer på ett SD-kort med SD-biblioteket. Här är hur man öppnar och läser en textfil:

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // Vänta på att Serial ska ansluta. Krävs för Leonardo endast.
  }

  if (!SD.begin(4)) {
    Serial.println("Initialisering misslyckades!");
    return;
  }
  
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Fel vid öppning av filen");
  }
}

void loop() {
  // Här kan din kod köra om och om igen.
}
```

Sample Output:
```
Hello, Arduino!
```

## Fördjupning
Arduino läste textfiler direkt från ett SD-kort redan omkring 2010 efter att SD-biblioteket blev tillgängligt. Alternativ till SD-biblioteket inkluderar EEPROM för mindre datamängder eller anslutning till en dator för större filer. Implementationen använder SPI (Serial Peripheral Interface) för att kommunicera med SD-kortet. Det är viktigt att förstå filsystemet (som FAT16 eller FAT32) som SD-kortet är formaterat med för att kunna navigera och läsa filer korrekt.

## Se även
- [Arduino SD library reference](https://www.arduino.cc/en/Reference/SD)
- [SPI library reference](https://www.arduino.cc/en/Reference/SPI)
