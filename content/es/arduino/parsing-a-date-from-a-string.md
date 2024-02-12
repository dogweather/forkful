---
title:                "Analizando una fecha a partir de una cadena de texto"
aliases:
- es/arduino/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:22.437188-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando una fecha a partir de una cadena de texto"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Analizar una fecha de una cadena en Arduino implica extraer y convertir los componentes de la fecha (año, mes, día) de una representación textual a un formato que se pueda utilizar para el mantenimiento del tiempo, comparaciones o manipulaciones dentro de los bocetos. Los programadores realizan frecuentemente esta tarea para interactuar con componentes como relojes en tiempo real, registradores, o para procesar la entrada de APIs web y interfaces de usuario donde las fechas podrían presentarse en un formato legible.

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
