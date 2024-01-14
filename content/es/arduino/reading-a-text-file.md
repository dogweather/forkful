---
title:                "Arduino: Lectura de un archivo de texto"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Arduino

Leer un archivo de texto en Arduino puede ser útil para almacenar datos o configuraciones que se necesiten para el funcionamiento del programa. Además, puede facilitar la manipulación de grandes cantidades de información en el código.

## Cómo hacerlo

Para leer un archivo de texto en Arduino, primero necesitamos usar la librería "SD" que nos permitirá interactuar con la tarjeta SD. A continuación, se deben seguir los siguientes pasos:

  ```Arduino
  // Incluimos la librería SD
  #include <SD.h>
  
  ...
  
  // Inicializamos la tarjeta SD en el pin 10
  if(!SD.begin(10)) {
    // En caso de error, mostramos un mensaje
    Serial.println("Error al inicializar la tarjeta SD");
    return;
  }
  
  // Abrimos el archivo de texto para lectura
  File archivo = SD.open("datos.txt");
  if(!archivo) {
    // En caso de error, mostramos un mensaje
    Serial.println("Error al abrir el archivo");
    return;
  }
  
  // Leemos el contenido del archivo y lo mostramos por el puerto serial
  while(archivo.available()) {
    Serial.write(archivo.read());
  }
  
  // Cerramos el archivo
  archivo.close();
  
  ```

El código anterior abre un archivo de texto llamado "datos.txt" y lo lee caracter por caracter, mostrándolo por el puerto serial. Para realizar operaciones más complejas con el contenido del archivo, se pueden utilizar diferentes métodos de la librería SD, como por ejemplo la función "readString()" para leer una cadena completa.

## Profundizando

Además de leer archivos de texto, la librería SD también nos permite crear y editar archivos, así como borrarlos o moverlos a diferentes ubicaciones en la tarjeta SD. También es importante tener en cuenta que el formato de la tarjeta SD debe ser FAT16 o FAT32 para que la lectura del archivo sea exitosa.

Utilizar archivos de texto en Arduino puede ser especialmente útil en proyectos que requieren la lectura de diferentes configuraciones y datos almacenados en la tarjeta SD. Algunos ejemplos de uso incluyen la configuración de parámetros en un robot o la visualización de datos en una pantalla LCD.

## Ver también

- [Documentación de la librería SD para Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutorial de Adafruit sobre el uso de la tarjeta SD con Arduino](https://learn.adafruit.com/adafruit-data-logger-shield?view=all)