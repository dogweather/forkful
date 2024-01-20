---
title:                "Kontrollera om en katalog finns"
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att kontrollera om en mapp finns är processen att verifiera om en viss mapp finns på din Arduino-enhet. Programmerare gör detta för att förhindra fel och misslyckade operationer när de försöker arbeta med en icke-existerande mapp.

## Hur man gör:
Här är ett grundläggande exempel på hur du kan kontrollera om en mapp finns på din SD-kort med Arduino:

```Arduino
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4;

void setup() {
  Serial.begin(9600);
  Serial.print("Initializing SD card...");
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Card Failed");
    return;
  }
  Serial.println("Card Initialized");
  
  if (SD.exists("/example")) {
    Serial.println("Directory Exists");
  } else {
    Serial.println("Directory Not Found");
  }
}

void loop() {
  // nothing 
}
```

Om mappen "example" finns på SD-kortet kommer outputten att vara "Directory Exists" annars kommer den att vara "Directory Not Found".

## Djupdykning
Historiskt sett fanns det en tid då att kontrollera om en mapp finns i Arduino inte var möjligt. Denna funktion introducerades med tillkomsten av SD-library, vilket öppnade dörrarna för programmerare att hantera filer och kataloger mer effektivt.

En alternativ metod för att kontrollera om en katalog finns är att försöka skapa eller öppna den och hantera alla fel som kan uppstå från operationen. Detta kanske inte alltid är det bästa alternativet eftersom vissa fel kan vara svåra att identifiera.

Implementera 'Directory exists'-kontrollen är relativt enkelt i Arduino tack vare SD library metoden SD.exists(). Denna metod returnerar helt enkelt true om mappen finns och false om den inte gör det.

## Se även
För mer information, se följande resurser på ämnet

- [Arduino - SD Library](https://www.arduino.cc/en/Reference/SD)
- [Arduino Stack Exchange](https://arduino.stackexchange.com/questions/578/how-to-check-if-a-file-exists)
  
Observera att dessa resurser är på engelska.