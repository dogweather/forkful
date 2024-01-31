---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
simple_title:         "Escritura de un archivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Escribir un archivo de texto en Arduino significa almacenar datos en una tarjeta SD o memoria. Los programadores lo hacen para registrar información, como datos de sensores o eventos del sistema.

## Cómo Hacerlo:
Para escribir en un archivo de texto necesitas un módulo SD y la librería `SD.h`.

```Arduino
#include <SPI.h>
#include <SD.h>

File miArchivo;
void setup() {
  Serial.begin(9600);
  // Comprueba si la tarjeta está presente y se puede inicializar:
  if (!SD.begin(10)) {
    Serial.println("Fallo al iniciar la SD");
    return;
  }

  // Abre el archivo, si no existe lo crea:
  miArchivo = SD.open("prueba.txt", FILE_WRITE);
  // Si el archivo se abrió correctamente, escribe en él:
  if (miArchivo) {
    miArchivo.println("Hola Mundo!");
    miArchivo.close(); // Cierra el archivo
  } else {
    // si el archivo no se abre, imprime un error:
    Serial.println("Error al abrir el archivo");
  }
}

void loop() {
  // Nada que hacer aquí
}
```

Salida esperada al abrir `prueba.txt`:

```
Hola Mundo!
```

## Inmersión Profunda:
La librería `SD.h` viene desde Arduino 1.0, permitiendo escritura/lectura de tarjetas SD utilizando el SPI. Alternativas incluyen EEPROM para guardar pequeñas cantidades de datos sin necesidad de hardware externo. Los detalles de implementación involucran gestionar correctamente la apertura y cierre de archivos para evitar corrupción de datos.

## Ver También:
- Documentación oficial de la librería SD: https://www.arduino.cc/en/Reference/SD
- Guía para el módulo de tarjeta SD: https://www.arduino.cc/en/Guide/MKRSD
- Tutorial sobre EEPROM en Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
