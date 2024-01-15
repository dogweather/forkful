---
title:                "Trabajando con csv."
html_title:           "Arduino: Trabajando con csv."
simple_title:         "Trabajando con csv."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

Si estás buscando una forma sencilla de almacenar y organizar grandes cantidades de datos en tu proyecto de Arduino, entonces trabajar con archivos CSV es una excelente opción. En este artículo, te mostraré cómo utilizar archivos CSV en tu programa de Arduino y por qué podría ser beneficioso para ti.

## ¿Por qué usar archivos CSV?

Los archivos CSV son una forma de almacenar y organizar datos en una estructura de tabla. Son ideales para manejar grandes cantidades de datos, ya que pueden ser fácilmente importados y exportados desde diferentes programas, como hojas de cálculo o bases de datos. Esto los hace muy útiles para proyectos Arduino que requieren almacenar y analizar datos de forma eficiente.

## Cómo utilizar archivos CSV en Arduino

Para trabajar con archivos CSV en Arduino, necesitarás la librería "SD.h" que viene incluida en el software de Arduino. También necesitarás una tarjeta SD y un lector de tarjetas SD para guardar y leer los archivos.

Primero, debes formatear la tarjeta SD en formato FAT16 o FAT32 para que sea compatible con la librería SD.h. Luego, inserta la tarjeta en el lector y conéctalo a tu Arduino.

A continuación, crea un nuevo archivo en blanco llamado "datos.csv" en la tarjeta SD y colócalo en la raíz. Puedes hacerlo manualmente o utilizar el siguiente código dentro de la función "setup()" de tu programa Arduino:

```arduino
#include <SD.h>

File archivo;

void setup(){
  Serial.begin(9600);
  if (!SD.begin(53)) {
    Serial.println("Error al montar la tarjeta SD");
    return;
  }
  archivo = SD.open("datos.csv", FILE_WRITE);
  if (archivo) {
    archivo.close();
    Serial.println("Archivo creado correctamente");
  } else {
    Serial.println("Error al crear archivo");
  }
}

void loop(){
  // tu código aquí
}
```

El código anterior comprueba si la tarjeta SD está funcionando correctamente y luego crea el archivo "datos.csv" en la raíz de la tarjeta SD. Como puedes ver, se utiliza la función "FILE_WRITE" para indicar que el archivo será utilizado para escribir datos.

Para escribir datos en el archivo, puedes utilizar la función "println()" que escribe una línea completa en el archivo. Por ejemplo:

```arduino
archivo = SD.open("datos.csv", FILE_WRITE);
archivo.println("Sensor 1, 10");
archivo.println("Sensor 2, 20");
archivo.println("Sensor 3, 30");
archivo.close();
```

Esto escribirá tres líneas en el archivo, cada una con dos valores separados por una coma. Ahora, si abres el archivo en un programa de hoja de cálculo, podrás ver los datos organizados en una tabla.

Para leer datos de un archivo CSV, puedes utilizar la función "readStringUntil()" que lee una línea completa del archivo hasta encontrar una coma. Puedes almacenar los valores leídos en variables y luego utilizarlos en tu código. Por ejemplo:

```arduino
archivo = SD.open("datos.csv");
String primerSensor = archivo.readStringUntil(',');
String valorPrimerSensor = archivo.readStringUntil('\n');
String segundoSensor = archivo.readStringUntil(',');
String valorSegundoSensor = archivo.readStringUntil('\n');
archivo.close();
```

Esto leerá la primera línea del archivo y almacenará el primer valor antes de la coma en la variable "primerSensor" y el segundo valor después de la coma en "valorPrimerSensor". Luego, la segunda línea se almacenará en las variables "segundoSensor" y "valorSegundoSensor", respectivamente.

## Profundizando en el uso de archivos CSV

Además de escribir y leer datos en un archivo CSV, también puedes realizar otras operaciones útiles, como borrar, renombrar o eliminar un archivo. Esto se hace mediante el uso de diferentes funciones de la librería SD.h.

Si quieres aprender más sobre cómo trabajar con archivos CSV y la librería SD.h, puedes consultar la documentación oficial de Arduino o realizar una búsqueda en línea para encontrar tutoriales y ejemplos prácticos.

## Ver también

- [Documentación oficial de Arduino sobre la librería SD.h](https://www.arduino.cc/en/Reference/SD)
- [Un tutorial detallado sobre cómo trabajar con archivos CSV en Arduino](https://howtomechatronics.com/tutorials/arduino/how-to-write-and-read-from-sd-card-in-arduino/)