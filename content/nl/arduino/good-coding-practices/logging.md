---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:36.678485-07:00
description: "\"Loggen\" is het bijhouden van een register van gebeurtenissen, transacties\
  \ of activiteiten die over tijd plaatsvinden in een systeem. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:51.078367-06:00'
model: gpt-4-0125-preview
summary: '"Loggen" is het bijhouden van een register van gebeurtenissen, transacties
  of activiteiten die over tijd plaatsvinden in een systeem.'
title: Logboekregistratie
weight: 17
---

## Hoe:
Arduino wordt niet geleverd met een ingebouwde logbibliotheek zoals sommige andere omgevingen, maar je kunt basaal loggen naar de seriële console implementeren met minimale moeite. Hier is een snel voorbeeld om je op weg te helpen:

```arduino
void setup() {
  // Start de seriële communicatie met de gegeven baudrate
  Serial.begin(9600);

  // Wacht tot de seriële poort verbinding maakt - alleen nodig op sommige borden
  while (!Serial) {
    ; // wacht tot de seriële poort verbinding maakt. Nodig voor native USB
  }

  // Log een informatief bericht dat aangeeft dat het setupproces is voltooid
  Serial.println("Setup voltooid!");
}

void loop() {
  // Eenvoudige logger die elke seconde de uptime print
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Uptime (ms): ");
    Serial.println(currentMillis);

    // Hier kun je ook foutlogs, waarschuwingen of andere info toevoegen.
  }
  
  // De rest van je programmalogica hier...
}
```

Voorbeeld van seriële uitvoer:
```
Setup voltooid!
Uptime (ms): 1000
Uptime (ms): 2000
Uptime (ms): 3000
...
```

## Diepgaand:
Historisch gezien was loggen op microcontrollers niet zo eenvoudig als op een volledig besturingssysteem. Beperkte middelen betekenden dat elke byte telde, en ontwikkelaars moesten voorzichtig zijn om het systeem niet te verstoppen. Met de komst van krachtigere borden en het Arduino-platform dat het proces vereenvoudigt, is loggen toegankelijker geworden.

Hoewel de code hierboven loggen via de seriële interface demonstreert, omvatten andere methoden het schrijven naar een SD-kaart, het verzenden van gegevens over een netwerk naar een externe server of zelfs het uitvoeren naar een kleine LCD.

Het implementeren van een logsysteem brengt overwegingen met zich mee zoals rotatie, ernstniveau (info, debug, waarschuwing, fout) en impact op de prestaties. Op een Arduino moet je mogelijk rekening houden met geheugenbeperkingen bij het loggen van complexe gegevensstructuren. Voor externe loggen is de beveiliging van de verzonden logs ook een zorg.

Er bestaan ​​geavanceerdere oplossingen zoals Syslog, een breed geadopteerde logstandaard, buiten de Arduino-wereld, maar je kunt externe bibliotheken integreren die soortgelijke functionaliteit bieden met verschillende graden van complexiteit en middelenvereisten.

## Zie ook:
- [Arduino 'Serial'-referentie](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD-kaart loggen met Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFun's Data Logging schild](https://www.sparkfun.com/products/13712)
- [TinyWeb: Een praktisch voorbeeld van extern loggen met Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
