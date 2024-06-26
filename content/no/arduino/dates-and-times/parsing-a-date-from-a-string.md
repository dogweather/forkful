---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:26.960869-07:00
description: "Hvordan: Direkte tiln\xE6rming uten et tredjepartsbibliotek."
lastmod: '2024-03-13T22:44:41.067292-06:00'
model: gpt-4-0125-preview
summary: "Direkte tiln\xE6rming uten et tredjepartsbibliotek."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
Direkte tilnærming uten et tredjepartsbibliotek:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Eksempel datostreng i YYYY-MM-DD format
  String dateString = "2023-04-01";

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Initier et DateTime-objekt med analyserte komponenter
  DateTime parsedDate(year, month, day);
  
  Serial.print("Analysert Dato: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Eksempel Utdata:
```
Analysert Dato: 2023/4/1
```

Bruk av et tredjepartsbibliotek (*ArduinoJson* for mer komplekse parsingsscenarier, som å skaffe en dato fra et JSON-svar):

Først, installer ArduinoJson-biblioteket gjennom Arduino Library Manager.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulerer et JSON-svar
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Trekker ut datostrengen
  const char* date = doc["date"];

  // Parser datoen fra strengen som før
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Analysert Dato fra JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Eksempel Utdata:
```
Analysert Dato fra JSON: 2023/7/19
```
