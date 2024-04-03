---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:27.453997-07:00
description: "L'analyse (parsing) d'une date \xE0 partir d'une cha\xEEne de caract\xE8\
  res dans Arduino consiste \xE0 extraire et \xE0 convertir les composants de la date\
  \ (ann\xE9e, mois,\u2026"
lastmod: '2024-03-13T22:44:58.123144-06:00'
model: gpt-4-0125-preview
summary: "L'analyse (parsing) d'une date \xE0 partir d'une cha\xEEne de caract\xE8\
  res dans Arduino consiste \xE0 extraire et \xE0 convertir les composants de la date\
  \ (ann\xE9e, mois, jour) d'une repr\xE9sentation textuelle en un format qui peut\
  \ \xEAtre utilis\xE9 pour la tenue du temps, les comparaisons ou les manipulations\
  \ dans les sketches."
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

## Comment faire :
Approche directe sans bibliothèque tierce :

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Exemple de chaîne de date au format AAAA-MM-JJ
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Initialiser un objet DateTime avec les composants analysés
  DateTime parsedDate(year, month, day);
  
  Serial.print("Date analysée : ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Exemple de sortie :
```
Date analysée : 2023/4/1
```

Utilisation d'une bibliothèque tierce (*ArduinoJson* pour des scénarios d'analyse plus complexes, tels que l'obtention d'une date à partir d'une réponse JSON) :

D'abord, installez la bibliothèque ArduinoJson via le Gestionnaire de bibliothèques Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulation d'une réponse JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extraction de la chaîne de date
  const char* date = doc["date"];

  // Analyser la date à partir de la chaîne comme précédemment
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Date analysée à partir du JSON : ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Exemple de sortie :
```
Date analysée à partir du JSON : 2023/7/19
```
