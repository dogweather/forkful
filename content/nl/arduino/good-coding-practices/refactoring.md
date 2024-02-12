---
title:                "Refactoring"
date:                  2024-01-28T22:05:57.582174-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herwerken van je code om zijn structuur en leesbaarheid te verbeteren zonder het externe gedrag of de functionaliteit te wijzigen. Programmeurs refactoren om hun code schoner, makkelijker te begrijpen en onderhoudbaarder te maken, wat op de lange termijn het debuggen en toevoegen van nieuwe functies veel minder hoofdpijn oplevert.

## Hoe:

Stel je voor dat je een functie op je Arduino hebt die veel te veel doet, zoals dit:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Een functie die te veel doet
  handleEverything();
}

void handleEverything() {
  // Lees sensorgegevens
  int sensorValue = analogRead(A0);
  // Verwerk de sensorgegevens
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Print de sensorgegevens
  Serial.println(sensorValue);
  delay(500);
}
```

Het refactoren ervan zou er zo uit kunnen zien dat `handleEverything()` wordt gesplitst in kleinere, meer gefocuste functies:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Na het refactoren is de `loop()` functie leesbaarder en elke taak wordt afgehandeld door een toegewijde functie, waardoor de code makkelijker te beheren is.

## Diepduiken
Historisch gezien werd refactoring populair met de opkomst van Agile en Testgedreven Ontwikkeling (TDD) methodologieën, die vertrouwen op constante codeverbetering om zich aan te passen aan veranderende vereisten. Er zijn verschillende tools en strategieën voor refactoring - zoals de “Extract Method” techniek die we in ons Arduino-voorbeeld gebruikten. Dit is essentieel wanneer je overgaat van een snel prototype naar een stabiel project, waarbij codeleesbaarheid en onderhoud cruciaal worden.

Bij het refactoren is het belangrijk om een goede set tests op zijn plaats te hebben om ervoor te zorgen dat wijzigingen geen bugs hebben geïntroduceerd. In de Arduino wereld is geautomatiseerd testen niet altijd eenvoudig vanwege hardware-afhankelijkheden, maar je kunt nog steeds unit testing gebruiken voor pure logica-onderdelen of simulatoren gebruiken.

Alternatieven voor handmatige refactoring omvatten het gebruik van dedicated refactoring-tools, die de identificatie van code smells automatiseren en wijzigingen voorstellen. Deze tools missen echter vaak de nuance voor microcontrollercode en zijn mogelijk niet beschikbaar in de Arduino-ontwikkelomgeving.

Uiteindelijk is refactoring een kunst die het verbeteren van de interne structuur van de code in evenwicht brengt tegen het risico van het introduceren van defecten. Het vereist dat je nadenkt over implementatiedetails zoals geheugengebruik en processortijd, vooral vanwege de bronbeperkte aard van microcontrollers.

## Zie Ook
Je kunt dieper duiken in refactoring met het baanbrekende boek van Martin Fowler *Refactoring: Improving the Design of Existing Code*. Voor een nadere blik op Arduino-specifieke praktijken, bekijk de Arduino-ontwikkelingsforums en -gemeenschappen:

- [Arduino Forum - Programmeringsvragen](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Onthoud, het doel is schone, begrijpelijke code waar toekomstige jij en anderen je dankbaar voor zullen zijn. Blijf hacken, en houd het netjes!
