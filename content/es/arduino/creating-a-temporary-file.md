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

## ¿Qué & Por qué?

Crear un archivo temporal en Arduino se refiere a crear un archivo que solo existirá temporalmente en la memoria del dispositivo. Los programadores suelen hacer esto cuando necesitan almacenar temporalmente datos o información que luego serán eliminados cuando ya no sean necesarios.

## Cómo:

### Ejemplo 1:

```Arduino
#include <SD.h> // incluir la biblioteca SD
File file; // inicializar un objeto File

void setup() {
  pinMode(10, OUTPUT); // configurar el pin 10 como salida
  SD.begin(4); // iniciar la comunicación con la tarjeta SD
  file = SD.open("temp.txt", FILE_WRITE); // crear un archivo temporal llamado "temp.txt" para escritura
  if (file) {
    file.println("Este es un ejemplo de archivo temporal en Arduino."); // escribir una línea de texto en el archivo
    file.close(); // cerrar el archivo
    digitalWrite(10, HIGH); // encender un LED para indicar que el archivo se creó exitosamente
  }
}

void loop() {
  // hacer otras cosas en el loop
}
```

El código anterior crea un archivo temporal en la tarjeta SD conectada al Arduino y escribe una línea de texto en él. Luego, cierra el archivo y enciende un LED para indicar que el archivo se creó exitosamente.

### Ejemplo 2:

```Arduino
#include <SPI.h> // incluir la biblioteca SPI
#include <SD.h> // incluir la biblioteca SD
File file; // inicializar un objeto File

void setup() {
  pinMode(4, OUTPUT); // configurar el pin 4 como salida
  SPI.begin(); // iniciar la comunicación con la tarjeta SD a través de SPI
  SD.begin(10); // iniciar la comunicación con la tarjeta SD
  file = SD.open("temp.txt", FILE_WRITE); // crear un archivo temporal llamado "temp.txt" para escritura
  if (file) {
    file.println("Este es otro ejemplo de archivo temporal en Arduino."); // escribir una línea de texto en el archivo
    file.close(); // cerrar el archivo
    digitalWrite(4, HIGH); // encender un LED para indicar que el archivo se creó exitosamente
  }
}

void loop() {
  // hacer otras cosas en el loop
}
```

Este código es similar al anterior, pero utiliza una conexión SPI para comunicarse con la tarjeta SD en lugar de una conexión directa.

## Inmersión Profunda:

Crear archivos temporales en Arduino es útil cuando se necesitan almacenar temporalmente datos o información para su posterior eliminación. En lugar de ocupar espacio en la memoria del dispositivo con datos innecesarios, se pueden utilizar archivos temporales para una mejor gestión de la memoria. Además, los archivos temporales también pueden ser utilizados como una forma de almacenar datos en caso de corte de energía repentino en el dispositivo.

Una alternativa al crear archivos temporales en Arduino es utilizar la función EEPROM para almacenar datos en la memoria interna del dispositivo. Sin embargo, esto puede ser más complicado ya que requiere la manipulación de direcciones de memoria.

La función open() utilizada en los ejemplos anteriores puede aceptar diferentes parámetros para especificar el modo de apertura del archivo (por ejemplo, solo lectura, solo escritura, o lectura y escritura) y la ubicación del archivo (por ejemplo, en la tarjeta SD o en la memoria interna). También es importante tener en cuenta que los archivos temporales deben ser eliminados manualmente después de su uso para evitar ocupar espacio innecesario en la memoria.

## Ver También:

- [Documentación de la biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Ejemplo de archivos temporales en Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Files/WriteBuf)