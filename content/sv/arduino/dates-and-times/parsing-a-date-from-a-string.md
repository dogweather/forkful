---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:26.613644-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng i Arduino inneb\xE4r att extrahera\
  \ och konvertera datumkomponenterna (\xE5r, m\xE5nad, dag) fr\xE5n en textuell representation\
  \ till\u2026"
lastmod: '2024-03-13T22:44:38.176636-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng i Arduino inneb\xE4r att extrahera\
  \ och konvertera datumkomponenterna (\xE5r, m\xE5nad, dag) fr\xE5n en textuell representation\
  \ till ett format som kan anv\xE4ndas f\xF6r tidsuppf\xF6ljning, j\xE4mf\xF6relser\
  \ eller manipulationer inom sketcher."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
Direkt metod utan tredjepartsbibliotek:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Exempel på datumsträng i YYYY-MM-DD-format
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Initiera ett DateTime-objekt med tolkade komponenter
  DateTime parsedDate(year, month, day);
  
  Serial.print("Tolkat Datum: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Exempel på utdata:
```
Tolkat Datum: 2023/4/1
```

Använda ett tredjepartsbibliotek (*ArduinoJson* för mer komplexa tolkningsscenarier, som att hämta ett datum från ett JSON-svar):

Först, installera ArduinoJson-biblioteket genom Arduino Library Manager.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulera ett JSON-svar
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extrahera datumsträngen
  const char* date = doc["date"];

  // Tolkar datumet från strängen som tidigare
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Tolkat Datum från JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Exempel på utdata:
```
Tolkat Datum från JSON: 2023/7/19
```
