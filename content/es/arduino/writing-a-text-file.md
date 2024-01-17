---
title:                "Escribiendo un archivo de texto"
html_title:           "Arduino: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto simplemente significa guardar información en un archivo de texto que se puede leer y editar fácilmente. Los programadores a menudo escriben archivos de texto como una forma de almacenar y organizar datos importantes o configuraciones para sus programas.

## Cómo hacerlo:

Para escribir un archivo de texto en Arduino, podemos usar la función "FileWrite" de la librería SD. Primero, debemos asegurarnos de tener una tarjeta SD conectada y montada en nuestro Arduino. Luego, podemos usar el siguiente código como ejemplo:

```Arduino
#include <SD.h>   // incluir la librería SD

File myFile;   // crear un objeto de tipo "File"

void setup() {
  SD.begin(4);  // montar la tarjeta en el pin 4
}

void loop() {
  myFile = SD.open("datos.txt", FILE_WRITE);  // abrir el archivo "datos.txt" en modo escritura
  if (myFile) {   // si el archivo se abrió correctamente
    for (int i = 0; i < 10; i++) {   // escribir 10 números en el archivo
      myFile.println(i);
    }
    myFile.close();   // cerrar el archivo
  }
  else {   // si el archivo no se pudo abrir correctamente
    Serial.println("Error al abrir el archivo");   // imprimir mensaje de error
  }
  delay(10000);   // esperar 10 segundos antes de volver a escribir en el archivo
}
```

El código anterior abre el archivo "datos.txt" en modo escritura y escribe los números del 0 al 9 en él. Luego, cierra el archivo y espera 10 segundos antes de volver a escribir en él. Si queremos agregar texto al final del archivo en lugar de sobrescribirlo, podemos usar la opción "FILE_APPEND" en lugar de "FILE_WRITE" al abrir el archivo.

La salida de este programa se puede ver en la ventana Serial del Arduino IDE o leyendo el archivo "datos.txt" desde la tarjeta SD.

## Profundizando:

Los archivos de texto han existido desde el inicio de la informática y también son muy utilizados en la programación. Sin embargo, existen otras formas de almacenar y organizar datos, como las bases de datos o los archivos binarios. Estos últimos son más eficientes en el uso del espacio y ofrecen una mayor seguridad y privacidad de los datos.

Para implementar la escritura de archivos de texto, Arduino usa la librería SD, que comunica la tarjeta SD con el microcontrolador a través del protocolo SPI. Esto permite una lectura y escritura rápida de datos.

## Véase también:

- Documentación oficial de Arduino sobre la librería SD: https://www.arduino.cc/en/Reference/SD
- Tutorial sobre cómo guardar datos en una tarjeta SD con Arduino: https://www.instructables.com/id/Arduino-Projects-Saving-several-sensor-values-to-a-/