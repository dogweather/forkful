---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:08.503347-07:00
description: "Een nieuw project starten op Arduino betekent het initialiseren van\
  \ een nieuwe schets, het canvas voor je code. Programmeurs doen dit om leven te\
  \ blazen\u2026"
lastmod: '2024-03-13T22:44:51.073387-06:00'
model: gpt-4-0125-preview
summary: "Een nieuw project starten op Arduino betekent het initialiseren van een\
  \ nieuwe schets, het canvas voor je code. Programmeurs doen dit om leven te blazen\u2026"
title: Een nieuw project starten
weight: 1
---

## Wat & Waarom?

Een nieuw project starten op Arduino betekent het initialiseren van een nieuwe schets, het canvas voor je code. Programmeurs doen dit om leven te blazen in nieuwe gadgets, van knipperende LED's tot robotica.

## Hoe:

```Arduino
// Maak een eenvoudige knipperschets om een nieuw Arduino-project te starten

void setup() {
  pinMode(LED_BUILTIN, OUTPUT); // Stel de ingebouwde LED in als uitvoer
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH); // Zet de LED aan
  delay(1000);                     // Wacht een seconde
  digitalWrite(LED_BUILTIN, LOW);  // Zet de LED uit
  delay(1000);                     // Wacht nog een seconde
}
```

Sluit je Arduino-bord aan, upload de schets en kijk hoe de ingebouwde LED elke seconde knippert.

## Diepgaand

Wanneer je aan een nieuw Arduino-project begint, volg je in de voetsporen van talloze uitvinders en knutselaars. Arduino begon in 2005 in Ivrea, Italië, als een hulpmiddel voor studenten zonder achtergrond in elektronica en programmeren. Sindsdien is het een basisgereedschap geworden in de doe-het-zelf-elektronica, prototyping en educatieve codering.

Er zijn alternatieven voor het vanaf nul starten van een project. Je kunt bestaande code aanpassen of bibliotheken gebruiken om complexe functies toe te voegen zonder het wiel opnieuw uit te vinden - maar niets verslaat de opwinding van het creëren van iets uniek eigens.

De schets begint met de functie `setup()`, die eenmaal draait om je hardware in te stellen, gevolgd door de functie `loop()`, die continu draait, waardoor je het gedrag van je project kunt controleren. Beheers het gebruik en de structuur van deze functies, en je bent goed op weg om een Arduino-pro te worden.

## Zie Ook

- Officiële Arduino-documentatie: https://www.arduino.cc/reference/en/
- Introductie tot Arduino-schetsen: https://www.arduino.cc/en/Tutorial/BuiltInExamples
- Arduino Forum – Projectbegeleiding: https://forum.arduino.cc/c/project-guidance/8
