---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:53.000466-07:00
description: "Het maken van een tijdelijk bestand betekent het cre\xEBren van een\
  \ bestand dat slechts voor korte tijd of voor de huidige sessie nodig is. Programmeurs\
  \ doen\u2026"
lastmod: '2024-02-25T18:49:48.424760-07:00'
model: gpt-4-0125-preview
summary: "Het maken van een tijdelijk bestand betekent het cre\xEBren van een bestand\
  \ dat slechts voor korte tijd of voor de huidige sessie nodig is. Programmeurs doen\u2026"
title: Een tijdelijk bestand aanmaken
---

{{< edit_this_page >}}

## Wat & Waarom?

Het maken van een tijdelijk bestand betekent het creëren van een bestand dat slechts voor korte tijd of voor de huidige sessie nodig is. Programmeurs doen dit om tussenliggende data op te slaan zonder de langetermijnopslag te belasten, of voor data die alleen nodig is terwijl het programma draait.

## Hoe:

Arduino werkt typisch met microcontrollers die niet over een traditioneel bestandssysteem beschikken - dus "bestanden" worden niet op dezelfde manier beheerd als op een pc. In plaats daarvan gebruiken we EEPROM (een kleine hoeveelheid geheugen die bewaard blijft na resets) of een SD-kaart met een shield. Hier is een basisvoorbeeld van het schrijven en lezen van tijdelijke gegevens naar EEPROM:

```Arduino
#include <EEPROM.h>

// Schrijf een tijdelijke waarde naar EEPROM
void writeTempEeprom(int address, byte value) {
  EEPROM.write(address, value);
}

// Lees een tijdelijke waarde uit EEPROM
byte readTempEeprom(int address) {
  return EEPROM.read(address);
}

void setup() {
  // Begin seriele communicatie
  Serial.begin(9600);

  // Schrijf naar en lees van EEPROM
  writeTempEeprom(0, 123); // Voorbeeldwaarde en -adres
  byte tempWaarde = readTempEeprom(0);

  // Toon de tijdelijke waarde
  Serial.print("Tijdelijke Waarde: ");
  Serial.println(tempWaarde);
}

void loop() {
  // Niets hier voor dit voorbeeld
}
```

En als je met een SD-kaart werkt:

```Arduino
#include <SPI.h>
#include <SD.h>

File tempBestand;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // Wacht tot de seriele poort verbinding maakt. Alleen nodig voor een native USB-poort
  }

  if (!SD.begin(4)) {
    Serial.println("Initialisatie mislukt!");
    return;
  }

  tempBestand = SD.open("temp.txt", FILE_WRITE);

  // Schrijf iets naar het tijdelijke bestand
  if (tempBestand) {
    tempBestand.println("Tijdelijke data tekst");
    tempBestand.close();
  } else {
    Serial.println("Fout bij het openen van temp.txt");
  }
  
  // Lees uit het tijdelijke bestand
  tempBestand = SD.open("temp.txt");
  if (tempBestand) {
    while (tempBestand.available()) {
      Serial.write(tempBestand.read());
    }
    tempBestand.close();
  } else {
    Serial.println("Fout bij het openen van temp.txt");
  }

  // Optioneel, verwijder het tijdelijke bestand na gebruik
  SD.remove("temp.txt");
}

void loop() {
  // Niets hier voor dit voorbeeld
}
```

Voorbeelduitvoer (voor beide voorbeelden) op de Seriële Monitor na het uitvoeren van de setup moet zijn:
```
Tijdelijke Waarde: 123
```
Of, voor het SD-kaartvoorbeeld:
```
Tijdelijke data tekst
```

## Diepgaande Duik

Historisch gezien dienen tijdelijke bestanden in de programmering voor zaken zoals caching, logs of interprocess-communicatie. Op systemen zoals pc's, met volledige besturingssystemen, zijn tempbestanden wijdverspreid. Bij Arduino is het anders. Microcontrollers hebben beperkte niet-vluchtige opslag (EEPROM), of we voegen externe opslag toe zoals SD-kaarten.

Alternatieven voor EEPROM voor kortetermijngegevens omvatten het gebruik van RAM (snel verloren tussen stroomcycli en reboots) of externe geheugen zoals Flash of een hard-wired IC.

Wat de implementatie betreft, als je naar EEPROM schrijft op een Arduino, onthoud dan dat het een beperkte schrijfcyclus heeft (vaak rond de 100.000 cycli). Het overmatig gebruiken kan het slijten - dus gebruik het spaarzaam voor werkelijk tijdelijke scenario's.

Een SD-kaart gebruiken voor tijdelijke opslag is vergelijkbaar met reguliere bestandsafhandeling op een pc. Het biedt meer ruimte, maar vereist een goed beheer zoals het waarborgen van kaarten van fatsoenlijke kwaliteit, het correct afhandelen van het openen/sluiten van bestanden, en het begrijpen dat het relatief langzaam is in vergelijking met EEPROM of RAM.

## Zie Ook

- [EEPROM Bibliotheek Referentie](https://www.arduino.cc/en/Reference/EEPROM)
- [SD Bibliotheek Referentie](https://www.arduino.cc/en/Reference/SD)
- [Arduino Bestands-I/O](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
- [Geheugen begrijpen](https://learn.adafruit.com/memories-of-an-arduino)
