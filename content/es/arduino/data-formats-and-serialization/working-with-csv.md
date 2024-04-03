---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:41.660723-07:00
description: "C\xF3mo hacerlo: Arduino no tiene una biblioteca incorporada espec\xED\
  ficamente para manejar archivos CSV, pero puedes usar las bibliotecas `SD` y `SPI`\
  \ para\u2026"
lastmod: '2024-03-13T22:44:59.356794-06:00'
model: gpt-4-0125-preview
summary: "Arduino no tiene una biblioteca incorporada espec\xEDficamente para manejar\
  \ archivos CSV, pero puedes usar las bibliotecas `SD` y `SPI` para acceder a archivos\
  \ en una tarjeta SD, y luego analizar o generar datos CSV usando t\xE9cnicas b\xE1\
  sicas de manipulaci\xF3n de cadenas."
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:
Arduino no tiene una biblioteca incorporada específicamente para manejar archivos CSV, pero puedes usar las bibliotecas `SD` y `SPI` para acceder a archivos en una tarjeta SD, y luego analizar o generar datos CSV usando técnicas básicas de manipulación de cadenas. Al tratar con manipulación de CSV más compleja, la biblioteca de terceros `ArduinoCSV` puede utilizarse para facilitar el análisis y la escritura.

**Leyendo datos CSV desde una tarjeta SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("¡Inicialización fallida!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Imprime la línea CSV
    }
    dataFile.close();
  } else {
    Serial.println("Error al abrir data.csv");
  }
}

void loop() {
  // No se usa en este ejemplo
}
```
*Salida de muestra:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Escribiendo datos CSV en una tarjeta SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("¡Inicialización fallida!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // Encabezado CSV
    dataFile.println("1, 1597840923, 23.5"); // Fila de datos de ejemplo
    dataFile.close();
    Serial.println("Datos escritos");
  } else {
    Serial.println("Error al abrir output.csv");
  }
}

void loop() {
  // No se usa en este ejemplo
}
```
*Salida de muestra:*
```
Datos escritos
```

**Usando ArduinoCSV para analizar:**
Si se manejan archivos CSV complejos, la biblioteca `ArduinoCSV` puede simplificar significativamente los esfuerzos de análisis. Este ejemplo asume que ya has instalado la biblioteca `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("¡Inicialización fallida!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Imprime cada campo
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Error al abrir data.csv");
  }
}

void loop() {
  // No se usa en este ejemplo
}
```
*Salida de muestra:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
En estos ejemplos, al leer y escribir en archivos CSV en una tarjeta SD, los proyectos de Arduino pueden recolectar datos fácilmente, almacenar configuraciones o intercambiar datos con otras aplicaciones en un formato universalmente accesible.
