---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:52.626293-07:00
description: "Hoe te: De verwachte uitvoer op de seri\xEBle monitor zal de inhoud\
  \ van `voorbeeld.txt` zijn als alles correct is aangesloten en ge\xEFnitialiseerd."
lastmod: '2024-04-05T21:53:51.103656-06:00'
model: gpt-4-0125-preview
summary: "De verwachte uitvoer op de seri\xEBle monitor zal de inhoud van `voorbeeld.txt`\
  \ zijn als alles correct is aangesloten en ge\xEFnitialiseerd."
title: Een tekstbestand lezen
weight: 22
---

## Hoe te:
```Arduino
#include <SPI.h>
#include <SD.h>

File mijnBestand;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wacht tot de seriële poort is verbonden.
  }

  if (!SD.begin(4)) {
    Serial.println("Initialisatie mislukt!");
    return;
  }

  mijnBestand = SD.open("voorbeeld.txt");
  if (mijnBestand) {
    while (mijnBestand.available()) {
      Serial.write(mijnBestand.read());
    }
    mijnBestand.close();
  } else {
    Serial.println("Fout bij het openen van voorbeeld.txt");
  }
}

void loop() {
  // er gebeurt niets na setup
}
```

De verwachte uitvoer op de seriële monitor zal de inhoud van `voorbeeld.txt` zijn als alles correct is aangesloten en geïnitialiseerd.

## Diepgaande Duik
Historisch gezien hadden microcontrollers zoals Arduino weinig geheugen en konden ze niet met bestanden omgaan. Maar met SD-kaartmodules en grotere ingebouwde geheugens, hebben we bestands-I/O gekregen. Er bestaan meerdere bibliotheken voor dit doel, zoals `<SD.h>`. Het is gebouwd bovenop `<SPI.h>` voor communicatie met de SD-kaart via de SPI-bus.

Wat betreft alternatieven, zou je EEPROM (niet-vluchtig geheugen) kunnen gebruiken voor kleine gegevens of zelfs een Arduino verbinden met een netwerk en bestanden ophalen van een server. De `<SD.h>` bibliotheek is een wrapper voor lagere functies, die bestandsbeheer, lezen en schrijven afhandelt op een manier die vergelijkbaar is met standaard C++ streams.

Implementatie op Arduino omvat het initialiseren van de SD-kaartmodule, het openen van het bestand, het lezen totdat er niets meer te lezen valt en dan sluiten om middelen vrij te geven. Het is essentieel om fouten te behandelen, zoals het niet kunnen initialiseren of openen van het bestand, omdat deze veelvoorkomende oorzaken zijn van problemen bij bestandsbewerkingen.

## Zie Ook
- Officiële SD-bibliotheekreferentie: https://www.arduino.cc/en/Reference/SD
- Arduino's SPI-bibliotheek voor seriële communicatie: https://www.arduino.cc/en/reference/SPI
- Gids voor het gebruik van EEPROM met Arduino voor kleinere gegevensopslagtaken: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
