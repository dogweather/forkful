---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:36.857580-07:00
description: "Controleren of een map bestaat gaat helemaal over het verifi\xEBren\
  \ van de aanwezigheid van een map op je opslag voordat je er iets mee doet. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:51.086305-06:00'
model: gpt-4-0125-preview
summary: "Controleren of een map bestaat gaat helemaal over het verifi\xEBren van\
  \ de aanwezigheid van een map op je opslag voordat je er iets mee doet. Programmeurs\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?
Controleren of een map bestaat gaat helemaal over het verifiëren van de aanwezigheid van een map op je opslag voordat je er iets mee doet. Programmeurs doen dit om fouten te voorkomen, zoals proberen een map te maken die er al is, of toegang proberen te krijgen tot een die er niet is.

## Hoe te:
Werken met mappen op Arduino omvat vaak het gebruik van de SD-bibliotheek voor opslag op een SD-kaart. Zorg eerst dat je Arduino correct is aangesloten op een SD-kaartmodule. Vervolgens gebruik je de `SD.exists()` functie om te controleren op het bestaan van een map. Hier is een snel voorbeeld:
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wachten tot de seriële poort verbinding maakt. Nodig alleen voor native USB-poort
  }

  if (!SD.begin(4)) { // Zorg ervoor dat je de juiste chip select pin gebruikt
    Serial.println("Initialisatie mislukt!");
    return;
  }

  if (SD.exists("/voorbeeld")) {
    Serial.println("/voorbeeld map bestaat.");
  } else {
    Serial.println("/voorbeeld map bestaat niet.");
  }
}

void loop() {
  // Niets te doen hier
}
```
Voorbeelduitvoer wanneer de map bestaat:
```
/voorbeeld map bestaat.
```
En wanneer het niet bestaat:
```
/voorbeeld map bestaat niet.
```
Vergeet niet `/voorbeeld` te vervangen met het daadwerkelijke pad dat je wilt controleren.

## Diepgaand
Lang geleden was het controleren op het bestaan van een map niet altijd eenvoudig. Systemen hadden uiteenlopende commando's. In het geval van Arduino, maakte de SD-bibliotheek het consistent, door concepten te lenen van standaard programmeerpraktijken.

Wat betreft alternatieven, als je werkt met niet-SD-opslag of meer controle nodig hebt, bieden andere bibliotheken zoals SdFat vergelijkbare functionaliteit met toegevoegde functies. Sommige geavanceerde implementaties kunnen directer met bestandssystemen interageren, maar voor de meeste gebruikers is SD.exists() genoeg.

Een map controleren houdt in dat de bibliotheek het bestandssysteem vraagt om een speciale bestandsinvoer op te zoeken die de map vertegenwoordigt. Als het er is, fantastisch. Zo niet, dan krijg je een false. De SD-bibliotheek handelt de laag-niveau communicatie af tussen je Arduino en het bestandssysteem van het opslagmedium, waardoor de lastige details worden geabstraheerd - zodat je de informatie krijgt die je nodig hebt zonder gedoe.

## Zie Ook
- Referentie van Arduino's SD-bibliotheek: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- SdFat-bibliotheek voor robuustere SD-kaartinteractie: [https://github.com/greiman/SdFat](https://github.com/greiman/SdFat)
