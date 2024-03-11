---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:59.542366-07:00
description: "Werken met CSV (Comma-Separated Values) in Arduino stelt je in staat\
  \ om gegevens als tekst op te slaan en te beheren. Het is goedkoop, eenvoudig en\u2026"
lastmod: '2024-03-11T00:14:24.929669-06:00'
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma-Separated Values) in Arduino stelt je in staat om\
  \ gegevens als tekst op te slaan en te beheren. Het is goedkoop, eenvoudig en\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met CSV (Comma-Separated Values) in Arduino stelt je in staat om gegevens als tekst op te slaan en te beheren. Het is goedkoop, eenvoudig en universeel, waardoor het ideaal is voor gegevensregistratie, configuratiebestanden of communicatie met spreadsheets en databases.

## Hoe:
Hier leest u hoe u sensordata kunt opslaan in een CSV-bestand op een SD-kaart:

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;
int sensorValue = analogRead(A0);  // gesimuleerde sensorwaarde

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {  // SD-kaart is verbonden met pin 4
    Serial.println("SD-kaartfout of niet aanwezig");
    return;
  }
  
  myFile = SD.open("data.csv", FILE_WRITE);
  
  if (myFile) {
    myFile.print("Tijd, SensorWaarde\n");
    unsigned long time = millis();
    myFile.print(time);
    myFile.print(", ");
    myFile.print(sensorValue);
    myFile.close();
    
    Serial.println("Gegevens geschreven naar SD-kaart.");
  } else {
    Serial.println("Fout bij het openen van bestand om te schrijven.");
  }
}

void loop() {
  // Hier is niets te doen
}
```

Voorbeeld van CSV-output in `data.csv`:
```
Tijd, SensorWaarde
12345, 678
```

## Diepgaand
Het CSV-formaat gaat terug tot de vroege dagen van de informatica. Hoewel er chiquere alternatieven zijn, zoals JSON of XML, blijft CSV de voorkeur hebben vanwege zijn eenvoud en brede ondersteuning op diverse platformen. Wanneer je met Arduino werkt, houd dan rekening met het beperkte geheugen en kies voor minimalistische CSV-bibliotheken of zelfgeschreven functies om CSV-gegevens efficiënt te parsen en te genereren.

## Zie Ook
- Arduino's SD-bibliotheekreferentie: https://www.arduino.cc/en/reference/SD
- Eenvoudig CSV-parsen in C: https://github.com/robertgamble/simplecsv
- Een tutorial over het opslaan van Arduino-gegevens in Excel: https://www.instructables.com/Save-Arduino-sensor-data-to-a-text-file/
