---
title:                "Escritura de un archivo de texto"
html_title:           "Arduino: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto en Arduino puede ser útil para guardar datos o para mostrar mensajes en una pantalla LCD. Además, te permite tener un registro de tus programas y cambios realizados en el código.

## Cómo hacerlo

Para escribir un archivo de texto en Arduino, debes seguir los siguientes pasos:

1. Abrir el programa Arduino y crear un nuevo sketch.
2. Agregar la librería SD al sketch utilizando la función ```#include <SD.h>```.
3. Inicializar la tarjeta SD utilizando la función ```SD.begin()```.
4. Abrir un archivo en la tarjeta SD utilizando la función ```SD.open()```.
5. Utilizar la función ```print()``` o ```println()``` para escribir en el archivo.
6. Cerrar el archivo utilizando la función ```SD.close()```.

Un ejemplo de código para escribir un archivo de texto sería el siguiente:

```Arduino
#include <SD.h>

void setup() {
  // Inicializar la tarjeta SD
  SD.begin();

  // Abrir el archivo "datos.txt" en la tarjeta SD
  File archivo = SD.open("datos.txt", FILE_WRITE);

  // Escribir en el archivo
  archivo.println("Ejemplo de texto en el archivo");
  archivo.println("Otro ejemplo de texto");

  // Cerrar el archivo
  archivo.close();
}

void loop() {
  // Código que se sigue ejecutando
}
```

El archivo "datos.txt" se guardará en la tarjeta SD y contendrá los dos mensajes escritos en el sketch.

## Profundizando

La función ```SD.open()``` acepta dos parámetros: el nombre del archivo y el modo de apertura. Los posibles modos son:

- ```FILE_READ```: para lectura
- ```FILE_WRITE```: para escritura (sobrescribe el contenido del archivo)
- ```FILE_APPEND```: para agregar al final del archivo

Además, la función ```print()``` o ```println()``` puede aceptar diferentes tipos de datos, no solo texto. Por ejemplo, puedes escribir variables numéricas o booleanas.

Ten en cuenta que debes utilizar una tarjeta SD formateada en formato FAT16 o FAT32 para poder escribir archivos en Arduino.

## See Also

- https://www.arduino.cc/en/Reference/SD
- https://www.arduino.cc/en/Tutorial/Files
- https://www.arduino.cc/en/Tutorial/Libraries