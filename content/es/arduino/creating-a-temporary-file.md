---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Crear un archivo temporal es el proceso de hacer un archivo que almacena datos de forma transitoria. Los programadores crean estos archivos para manejar el almacenamiento intermedio y facilitar la manipulación de datos.

## Cómo hacerlo:

A continuación, se muestra un código simple para escribir en un archivo temporal usando una tarjeta SD con Arduino. Asegúrese de conectar correctamente su lector de tarjetas SD a su Arduino.

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inicialización fallida!");
    return;
  }
  myFile = SD.open("temp.txt", FILE_WRITE);

  if (myFile) {
    myFile.println("Este es un archivo temporal");
    myFile.close();
  } else {
    Serial.println("Error abriendo temp.txt");
  }
}

void loop() {

}
```

Cuando ejecutes este código, crearás un archivo llamado 'temp.txt' en la tarjeta SD. Este archivo contendrá la línea "Este es un archivo temporal".

## Detalles profundos:

Históricamente, los archivos temporales han existido desde los primeros días de la computación para resolver problemas de almacenamiento. A pesar de que Arduino no tiene un sistema operativo con funcionalidades completas de manejo de archivos, este problema se puede solucionar usando una tarjeta SD y la biblioteca SD proporcionada por Arduino.

Una alternativa a los archivos temporales puede ser el uso de la memoria RAM a través de las variables globales. Sin embargo, esto puede ser limitado en sistemas como Arduino debido a la escasa cantidad de RAM disponible. 

La implementación detallada del manejo de archivos en Arduino se maneja en gran parte a través de la biblioteca SD. Esta biblioteca proporciona una serie de funciones para abrir, leer, escribir y cerrar archivos. Antes de usarla, el puerto del lector de tarjetas SD debe inicializarse con `SD.begin()`.

## Recursos adicionales:

Para información más detallada sobre la programación de archivos en Arduino, consulte los siguientes enlaces:

- Documentación de la biblioteca [SD](https://www.arduino.cc/en/reference/SD)
- Guía del usuario de Arduino en los [archivos](https://www.arduino.cc/en/Tutorial/Files)