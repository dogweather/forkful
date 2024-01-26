---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för "Comma-Separated Values" och används för att lagra tabulär data i enkla textfiler. Programutvecklare använder CSV för enkelhet, kompatibilitet med kalkylprogram och för att enkelt importera/exportera data från olika program.

## Hur man gör:
```arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  if (SD.exists("data.csv")) {
    Serial.println("data.csv exists.");
  } else {
    Serial.println("data.csv doesn't exist, creating file...");
    myFile = SD.open("data.csv", FILE_WRITE);
    myFile.println("SensorID,Temperature,Humidity"); // Header
    myFile.close();
  }
}

void loop() {
  myFile = SD.open("data.csv", FILE_WRITE);
  if (myFile) {
    myFile.print("1,");
    myFile.print("22.5,");
    myFile.println("45.0");
    Serial.println("Data written to data.csv");
    myFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
  delay(2000); // Wait for 2 seconds
}
```
Sample output till serial monitor:
```
data.csv exists.
Data written to data.csv
```

## Fördjupning:
CSV-formatet har sitt ursprung från 1970-talet där det användes i tidiga program för datainmatning och -bearbetning. Alternativ till CSV inkluderar JSON, XML, och databaser som SQL, men CSV används fortfarande på grund av sin enkelhet och breda stöd. När du implementerar CSV med Arduino bör du vara medveten om konsekvenser med filstorleken och strukturen då Arduinos minne är begränsat.

## Se även:
- Arduino SD-bibliotekets dokumentation: https://www.arduino.cc/en/Reference/SD
- CSV på Wikipedia: https://sv.wikipedia.org/wiki/CSV_(filformat)
- Guide till JSON med Arduino: https://arduinojson.org/
