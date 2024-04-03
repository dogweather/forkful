---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:35.669996-07:00
description: "Werken met JSON (JavaScript Object Notation) houdt in dat je gegevens\
  \ manipuleert die zijn gestructureerd in een lichtgewicht, op tekst gebaseerd formaat\u2026"
lastmod: '2024-03-13T22:44:51.093138-06:00'
model: gpt-4-0125-preview
summary: Werken met JSON (JavaScript Object Notation) houdt in dat je gegevens manipuleert
  die zijn gestructureerd in een lichtgewicht, op tekst gebaseerd formaat dat gemakkelijk
  te lezen en te schrijven is voor mensen, en eenvoudig te ontleden en te genereren
  voor machines.
title: Werken met JSON
weight: 38
---

## Hoe:
Om met JSON in Arduino te werken, heb je de ArduinoJson-bibliotheek nodig. Installeer het via Bibliotheekbeheer: Schets > Bibliotheek Includeren > Beheer Bibliotheken... zoek dan naar "ArduinoJson" en installeer.

Hier is een eenvoudig voorbeeld om JSON te ontleden:

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double breedtegraad = doc["data"][0];
  double lengtegraad = doc["data"][1];
  
  Serial.print("Sensor: ");
  Serial.println(sensor);
  Serial.print("Tijd: ");
  Serial.println(time);
  Serial.print("Breedtegraad: ");
  Serial.println(breedtegraad, 6);
  Serial.print("Lengtegraad: ");
  Serial.println(lengtegraad, 6);
}

void loop() {
  // Niet gebruikt in dit voorbeeld.
}
```

Voorbeelduitvoer:

```
Sensor: gps
Tijd: 1351824120
Breedtegraad: 48.756080
Lengtegraad: 2.302038
```

JSON creëren:

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);

  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  doc["data"][0] = 48.756080;
  doc["data"][1] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // Niet gebruikt in dit voorbeeld.
}
```

Voorbeelduitvoer:

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

## Diepere Duik
De ArduinoJson-bibliotheek, van Benoit Blanchon, werd de de facto standaard voor JSON-manipulatie in Arduino. JSON won aan populariteit vanwege de eenvoud ten opzichte van XML, dat voorheen veel gebruikt werd. Alternatieven zoals MsgPack bestaan, maar JSON blijft favoriet vanwege de leesbaarheid van de tekst en het wijdverbreide gebruik. Zorg bij de implementatie dat je voldoende geheugen toewijst aan de `DynamicJsonDocument` om overlopen te voorkomen en gebruik `StaticJsonDocument` voor statische of bekende JSON-objectgroottes.

## Zie Ook
- ArduinoJson-bibliotheekdocumentatie: https://arduinojson.org/
- Officiële website JSON: https://www.json.org/json-en.html
- Arduino-forum voor discussies: https://forum.arduino.cc/
- Gids voor het kiezen tussen StaticJsonDocument en DynamicJsonDocument: https://arduinojson.org/documentation/memory-model/
