---
title:                "Analyser une date depuis une chaîne de caractères"
aliases:
- fr/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:27.453997-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyser une date depuis une chaîne de caractères"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'analyse (parsing) d'une date à partir d'une chaîne de caractères dans Arduino consiste à extraire et à convertir les composants de la date (année, mois, jour) d'une représentation textuelle en un format qui peut être utilisé pour la tenue du temps, les comparaisons ou les manipulations dans les sketches. Les programmeurs effectuent fréquemment cette tâche pour interagir avec des composants comme les horloges en temps réel, les enregistreurs ou pour traiter l'entrée provenant des API web et des interfaces utilisateur où les dates peuvent être présentées dans un format lisible.

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
