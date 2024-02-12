---
title:                "Commandoregelargumenten lezen"
aliases:
- /nl/arduino/reading-command-line-arguments.md
date:                  2024-01-28T22:05:16.332326-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het lezen van command line argumenten betekent het ophalen van de data die naar een programma wordt doorgegeven wanneer je het start vanuit een terminal of command prompt. Programmeurs gebruiken argumenten om het gedrag van een programma aan te passen zonder de code te wijzigen.

## Hoe:
Arduino doet niet aan command line argumenten zoals traditionele programmeeromgevingen, omdat schetsen worden geüpload naar microcontrollers zonder een toegankelijke OS command line. Maar je kunt deze functie nabootsen door middel van seriële communicatie. Hier is hoe:

```arduino
void setup() {
  // Initialiseer seriële communicatie op 9600 bits per seconde:
  Serial.begin(9600);
}

void loop() {
  // Controleer of er gegevens beschikbaar zijn om te lezen.
  if (Serial.available() > 0) {
    // Lees de binnenkomende bytes totdat een nieuwe regel wordt ontvangen.
    String receivedData = Serial.readStringUntil('\n');
    // Echo de ontvangen argumenten terug naar de seriële monitor.
    Serial.print("Ontvangen: ");
    Serial.println(receivedData);
  }
}
```

Voorbeeld van Seriële Monitoruitvoer:
```
Ontvangen: argument1 argument2 argument3
```

## Uitgebreide Toelichting
Traditionele command line argumenten werken waar een volwaardig besturingssysteem (zoals Windows, Linux of macOS) programma's draait. De command processor van het OS geeft argumenten door aan programma's. Arduino heeft dit niet; het is een microcontroller met een enkel programma dat herhaaldelijk draait.

Seriële communicatie is je omweg. Het is alsof je een gesprek hebt met je Arduino over een speciale lijn. Je stuurt gegevens over deze lijn, die het Arduino-programma leest als invoer wanneer het klaar is.

Voor de Seriële Monitor in de Arduino IDE gebruikten programmeurs fysieke schakelaars of jumpers op de hardware om gedrag te wijzigen. Seriële communicatie was een gamechanger, die dit proces enorm vereenvoudigde.

Onthoud dat de Arduino Uno en vele anderen slechts één seriële poort hebben die gedeeld wordt met de USB-verbinding, wat betekent dat je geen seriële gegevens kunt ontvangen en tegelijkertijd een nieuwe schets kunt uploaden. Meer geavanceerde Arduino-borden kunnen meerdere seriële poorten hebben, wat gelijktijdige communicatie en schetsuploading mogelijk maakt.

Alternatieven voor seriële communicatie om command line argumenten na te bootsen, zijn:

- Bluetooth-modules (voor draadloze communicatie).
- Toetsenborden of knoppen voor invoer.
- Argumenten opslaan in EEPROM (niet-vluchtig geheugen) en ze bij het opstarten lezen.

Elke methode heeft zijn gebruikssituatie en complexiteitsniveau, maar seriële is het eenvoudigst voor snelle prototyping en testen.

## Zie Ook
- Arduino Seriële Communicatie: [Arduino - Seriële communicatie](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- Arduino EEPROM Lezen en Schrijven: [Arduino - EEPROM](https://www.arduino.cc/en/Reference/EEPROM)
