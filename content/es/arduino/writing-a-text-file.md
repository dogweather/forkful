---
title:    "Arduino: Escritura de un archivo de texto"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto puede ser una tarea muy útil para los programadores de Arduino. Esto nos permite almacenar información de una manera estructurada y accesible para nuestro programa. Además, puede ser útil para el almacenamiento de datos específicos o para la comunicación con otros dispositivos.

## Cómo hacerlo

Para escribir un archivo de texto en Arduino, necesitaremos usar la función `SD.open()` para abrir el archivo y luego utilizar la función `print()` o `println()` para escribir el contenido en él. Aquí hay un ejemplo de código: 

```
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10);

  myFile = SD.open("datos.txt", FILE_WRITE); //abrimos el archivo en modo escritura

  if (myFile) {
    myFile.println("¡Hola, mundo!"); //escribimos en el archivo
    myFile.print("El valor del sensor es: ");
    myFile.println(10); //también podemos escribir variables
    myFile.close(); //cerramos el archivo
  } else {
    Serial.println("Error al abrir el archivo."); //si hay un error, lo imprimimos por el puerto serial
  }
}

void loop() {
  //nada por hacer en el loop
}
```

Después de cargar el código en Arduino y abrir el monitor serial, deberíamos ver un mensaje de confirmación de que el archivo se ha creado correctamente. Además, si revisamos la tarjeta SD con una computadora, deberíamos encontrar un archivo llamado "datos.txt" con el contenido que hemos escrito.

## Profundizando

Escribir un archivo de texto en Arduino es un proceso relativamente simple, pero hay algunos detalles a tener en cuenta.

- Para usar la función `SD.open()`, necesitaremos incluir la librería `SD` en nuestro código.

- No podemos escribir directamente en la tarjeta SD. Primero debemos abrir el archivo y luego cerrarlo cuando hayamos terminado de escribir.

- Podemos especificar el modo de apertura del archivo (escritura, lectura, anexar, etc.) como segundo parámetro de la función `SD.open()`.

- Podemos usar la función `SD.exists()` para verificar si el archivo ya existe antes de abrirlo.

- También podemos utilizar la función `SD.mkdir()` para crear una carpeta en la tarjeta SD si es necesario.

¡Ahora ya sabes cómo escribir archivos de texto en Arduino!

## Ver también

- Documentación oficial sobre la función `SD.open()`: https://www.arduino.cc/en/Reference/SDOpen
- Tutorial sobre el manejo de archivos en una tarjeta SD: https://aprendiendoarduino.wordpress.com/2014/05/31/manejo-de-ficheros-en-sd-con-arduino/

¡Diviértete experimentando con la escritura de archivos en tu proyecto de Arduino!