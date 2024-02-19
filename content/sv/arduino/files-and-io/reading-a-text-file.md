---
aliases:
- /sv/arduino/reading-a-text-file/
date: 2024-01-20 17:53:39.884379-07:00
description: "L\xE4sa en textfil inneb\xE4r att extrahera dess inneh\xE5ll f\xF6r\
  \ vidare bearbetning eller visning. Programmerare g\xF6r detta f\xF6r att hantera\
  \ konfigurationer, lagra\u2026"
lastmod: 2024-02-18 23:08:52.060444
model: gpt-4-1106-preview
summary: "L\xE4sa en textfil inneb\xE4r att extrahera dess inneh\xE5ll f\xF6r vidare\
  \ bearbetning eller visning. Programmerare g\xF6r detta f\xF6r att hantera konfigurationer,\
  \ lagra\u2026"
title: "L\xE4sa en textfil"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsa en textfil innebär att extrahera dess innehåll för vidare bearbetning eller visning. Programmerare gör detta för att hantera konfigurationer, lagra data eller bara som en del av en större datahantering.

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
