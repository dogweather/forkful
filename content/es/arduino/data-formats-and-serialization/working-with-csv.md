---
title:                "Trabajando con CSV"
aliases:
- es/arduino/working-with-csv.md
date:                  2024-02-03T19:18:41.660723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con archivos CSV (Valores Separados por Comas) en Arduino implica leer y escribir en archivos CSV generalmente almacenados en una tarjeta SD, lo que permite el registro de datos, la configuración de ajustes y más. Los programadores a menudo manejan archivos CSV para la recolección de datos de sensores, almacenamiento de parámetros de configuración o interfaz con otros sistemas, debido a su simplicidad y amplia adopción a través de plataformas.

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
