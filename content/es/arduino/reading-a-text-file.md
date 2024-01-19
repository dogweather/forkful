---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

**## ¿Qué y Por qué?**

Leer un archivo de texto es el proceso de recuperar datos que se han guardado en un formato de texto. Los programadores lo hacen para manejar y analizar datos que no se generan en tiempo real.

**## Cómo hacerlo:**

Aquí tienes un ejemplo sencillo para leer un archivo de texto usando Arduino. Supon que tienes un archivo de texto llamado "ejemplo.txt".

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(4);
  
  myFile = SD.open("ejemplo.txt");

  while (myFile.available()) {
    Serial.write(myFile.read());
  }

  myFile.close();
}
```
La salida será el contenido de "ejemplo.txt" en el monitor serial.

**## Inmersión Profunda:**

1. Contexto histórico: La lectura de archivos de texto ha sido una habilidad básica en programación desde los inicios de los lenguajes de alto nivel.
2. Alternativas: Además de la biblioteca SD, podrías usar también la librería SPIFFS o incluso EEPROM si el tamaño de los datos es pequeño.
3. Detalles de implementación: Arduino lee los datos carácter por carácter, y se necesitará un búfer si se espera manejar grandes bloques de datos.

**## Ver También:**

https://www.arduino.cc/en/Reference/FileOpen - Referencia oficial para abrir archivos con Arduino.

https://www.arduino.cc/en/Reference/SD - Documentación oficial de la librería SD.

https://www.arduino.cc/en/Tutorial/Files - Un tutorial para manejar archivos en Arduino.