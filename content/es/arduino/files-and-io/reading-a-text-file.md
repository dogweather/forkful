---
date: 2024-01-20 17:53:40.860012-07:00
description: "C\xF3mo hacerlo: **Salida de muestra:**."
lastmod: '2024-04-05T21:54:00.698419-06:00'
model: gpt-4-1106-preview
summary: '**Salida de muestra:**.'
title: Lectura de un archivo de texto
weight: 22
---

## Cómo hacerlo:
```Arduino
#include <SPI.h>
#include <SD.h>

File miArchivo;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // esperar a que se inicialice el puerto serial
  }

  if (!SD.begin(4)) {
    Serial.println("Error al inicializar la SD");
    return;
  } else {
    Serial.println("SD inicializada correctamente");
  }
  
  miArchivo = SD.open("ejemplo.txt");
  if (miArchivo) {
    Serial.println("Contenido del archivo:");
    while (miArchivo.available()) {
      Serial.write(miArchivo.read());
    }
    miArchivo.close();
  } else {
    Serial.println("Error al abrir el archivo");
  }
}

void loop() {
  // nada aquí
}
```
**Salida de muestra:**
```
SD inicializada correctamente
Contenido del archivo:
Hola, esto es un texto de ejemplo
```

## Profundizamos:
La posibilidad de leer archivos de texto con Arduino viene de lejos. Antes, con menos memoria y poder de procesamiento, esto era más desafiante. Hoy, con módulos como la tarjeta SD, se simplifica. Alternativas incluyen EEPROM o incluso la nube. La implementación depende del almacenamiento. En SD, usas la biblioteca SD.h; en EEPROM, usas EEPROM.h. Los detalles importantes incluyen saber manejar correctamente la apertura y cierre de archivos para no dañarlos y cómo moverse a través de los datos.

## Vea También:
- [Arduino - SD Library](https://www.arduino.cc/en/Reference/SD)
- [SPI - Serial Peripheral Interface](https://www.arduino.cc/en/Reference/SPI)
