---
title:                "Skapande av en temporär fil"
html_title:           "Arduino: Skapande av en temporär fil"
simple_title:         "Skapande av en temporär fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

## Hur man gör

Att skapa en temporär fil kan vara användbart för att spara data temporärt, till exempel när du behöver lagra data under en kort period eller behöver tillfälligt använda ett program som behöver tillgång till en fil. För att skapa en temporär fil med Arduino behöver du använda funktionen `File::createTemp()`.

```Arduino
#include <FileIO.h>

void setup() {
  Serial.begin(9600);
  
  // Skapar en temporär fil med namnet "temp.txt"
  File tempFile = File.createTemp("temp.txt");
  
  // Skriver något till den temporära filen
  tempFile.println("Det här är en temporär fil");
  
  // Stänger filen
  tempFile.close();
  
  // Öppnar den temporära filen i läsläge och skriver ut innehållet
  File tempFileRead = File.open("temp.txt", FILE_READ);
  Serial.println("Innehållet i den temporära filen:");
  while (tempFileRead.available()) {
    Serial.write(tempFileRead.read());
  }
  tempFileRead.close();
}

void loop() {
  // Tom loop, ingen kod behövs i detta fall
}
```

Output:

```
Innehållet i den temporära filen:
Det här är en temporär fil
```

## En djupdykning

Funktionen `File::createTemp()` skapar en temporär fil som sparas i ett speciellt område i Arduinos minne, vilket gör att filen inte påverkar eventuella andra filer som finns på ditt SD-kort. Denna funktion returnerar en `File`-klass som du kan använda för att hantera den temporära filen, precis som du skulle göra med vilken annan fil som helst.

Det finns också en möjlighet att tillhandahålla ett andra argument till funktionen `File::createTemp()`, som bestämmer storleken på den temporära filen i antal bytes. Om detta argument inte anges kommer den temporära filen att ha en standardstorlek som beror på din Arduino-modell.

## Se även

- [Dokumentation för FileIO.h](https://arduino.github.io/arduino-library-api/filesystem_8h_source.html)
- [Guide för att arbeta med filer på SD-kort](https://www.arduino.cc/en/Reference/SD)
- [Arduino for Dummies (swedish)](https://www.education.com/download/worksheet/99858/arduino-dummies.pdf)