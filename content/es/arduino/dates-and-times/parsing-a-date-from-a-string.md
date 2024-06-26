---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:22.437188-07:00
description: "C\xF3mo hacerlo: Enfoque directo sin una biblioteca de terceros."
lastmod: '2024-03-13T22:44:59.342548-06:00'
model: gpt-4-0125-preview
summary: Enfoque directo sin una biblioteca de terceros.
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:
Enfoque directo sin una biblioteca de terceros:

```cpp
#include <Wire.h>
#include <RTClib.h>

void setup() {
  Serial.begin(9600);
  // Ejemplo de cadena de fecha en formato AAAA-MM-DD
  String dateString = "2023-04-01"; 

  int year = dateString.substring(0, 4).toInt();
  int month = dateString.substring(5, 7).toInt();
  int day = dateString.substring(8, 10).toInt();

  // Inicializa un objeto DateTime con los componentes analizados
  DateTime parsedDate(year, month, day);
  
  Serial.print("Fecha Analizada: ");
  Serial.print(parsedDate.year(), DEC);
  Serial.print("/");
  Serial.print(parsedDate.month(), DEC);
  Serial.print("/");
  Serial.println(parsedDate.day(), DEC);
}

void loop() {}
```

Salida de ejemplo:
```
Fecha Analizada: 2023/4/1
```

Usando una biblioteca de terceros (*ArduinoJson* para escenarios de análisis más complejos, como la obtención de una fecha de una respuesta JSON):

Primero, instala la biblioteca ArduinoJson a través del Administrador de Bibliotecas de Arduino.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Simulando una respuesta JSON
  String jsonResponse = "{\"date\":\"2023-07-19\"}";
  StaticJsonDocument<200> doc;
  deserializeJson(doc, jsonResponse);

  // Extrayendo la cadena de fecha
  const char* date = doc["date"];

  // Analiza la fecha de la cadena como antes
  int year = String(date).substring(0, 4).toInt();
  int month = String(date).substring(5, 7).toInt();
  int day = String(date).substring(8, 10).toInt();
  
  Serial.print("Fecha Analizada desde JSON: ");
  Serial.print(year);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(day);
}

void loop() {}
```

Salida de ejemplo:
```
Fecha Analizada desde JSON: 2023/7/19
```
