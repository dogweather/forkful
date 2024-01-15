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

## ¿Por qué crear un archivo temporal en Arduino?

Hay varias razones por las que podrías querer crear un archivo temporal en Arduino. Algunas posibles aplicaciones son guardar datos sensibles temporalmente, almacenar información durante una sesión de ejecución o incluso para realizar pruebas antes de guardar datos permanentes.

## Cómo hacerlo

Crear un archivo temporal en Arduino es bastante sencillo. A continuación, se presenta un código de ejemplo y su salida en un bloque de código ```Arduino```:

```
#include <SPI.h> //Librería necesaria para crear archivos temporales
#include <SD.h> //Librería necesaria para el manejo de archivos en una tarjeta SD

File tempFile; //Variable para almacenar el archivo temporal

void setup() {
  //Inicializar la comunicación con la tarjeta SD
  SD.begin(4); //4 representa el pin CS de la tarjeta SD
}

void loop() {
  //Crear el archivo temporal en la tarjeta SD
  tempFile = SD.open("temp.txt", FILE_WRITE);

  //Escribir datos en el archivo
  tempFile.println("Este es un archivo temporal creado en Arduino.");

  //Cerrar el archivo
  tempFile.close();

  //Esperar 1 segundo antes de ejecutar el proceso nuevamente
  delay(1000);
}
```

Salida:

```
Este es un archivo temporal creado en Arduino.
```

## Profundizando

Ahora que sabes cómo crear un archivo temporal en Arduino, es importante tener en cuenta algunos detalles. Primero, es necesario tener una tarjeta SD conectada al Arduino para poder guardar el archivo temporal. Además, es importante cerrar el archivo después de utilizarlo para evitar problemas de escritura y lectura en la tarjeta SD.

También es importante tener en cuenta que los archivos temporales se guardarán en la tarjeta SD hasta que se eliminen manualmente o el Arduino se apague. Si se desea guardar los datos permanentemente, se deberá programar para que el Arduino copie los datos del archivo temporal a un archivo permanente antes de ser eliminado.

## Ver también

- [Tutorial de Arduino sobre el manejo de tarjetas SD](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Documentación oficial de la librería SD](https://www.arduino.cc/en/Reference/SD)
- [Ejemplo de creación de archivos temporales en Arduino](https://www.arduino.cc/en/Tutorial/CreateTempFile)