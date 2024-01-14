---
title:                "Arduino: Creando un archivo temporal"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Arduino?

Crear un archivo temporal es útil cuando necesitas almacenar datos temporales que no necesitas guardar permanentemente o cuando quieres realizar pruebas en tu programa sin alterar los datos originales. Esto te permite realizar cambios y experimentar sin afectar el funcionamiento de tu código.

## Cómo crear un archivo temporal en Arduino

Crear un archivo temporal en Arduino es sencillo. Primero, necesitas incluir la librería SD en tu programa, la cual te permitirá trabajar con archivos en una tarjeta SD. Luego, puedes seguir los siguientes pasos:

  * Abre una conexión con la tarjeta SD mediante `SD.begin()`
  * Usa la función `SD.open()` para crear un archivo y asignarle un nombre y una extensión
  * Utiliza la función `file.println()` para escribir datos en el archivo
  * Cierra el archivo con `file.close()` cuando hayas terminado de escribir en él

```
#include <SD.h>

void setup() {
  // conexión con la tarjeta SD
  SD.begin();

  // crear un archivo temporal
  File file = SD.open("temp.txt", FILE_WRITE);

  // escribir datos en el archivo
  file.println("Esto es un archivo temporal en Arduino");
  file.println("Puedes escribir tantas líneas como quieras");

  // cerrar el archivo
  file.close();
}
```

## Profundizando en la creación de archivos temporales

Crear un archivo temporal en Arduino sigue los mismos principios que crear un archivo permanente. Sin embargo, es importante tener en cuenta algunos detalles:

  * El archivo se guarda en la tarjeta SD, por lo que es importante asegurarse de tener una tarjeta SD válida y conectada.
  * No debes usar el mismo nombre para tus archivos temporales y permanentes, ya que podrían sobreescribirse.
  * Puedes elegir la extensión que desees para tu archivo temporal, pero es recomendable utilizar algo que te ayude a identificarlo fácilmente, como `.tmp`.

Recuerda que los archivos temporales se eliminan automáticamente una vez que tu programa se reinicia o la tarjeta SD se desconecta, por lo que debes asegurarte de guardar los datos importantes en un archivo permanente antes de eso.

## Véase también

  * [Documentación de la librería SD en Arduino](https://www.arduino.cc/en/Reference/SD)
  * [Tutorial: Cómo trabajar con archivos en una tarjeta SD](https://learn.sparkfun.com/tutorials/sd-cards-and-arduino)
  * [Ejemplos de código para crear archivos temporales en Arduino](https://github.com/arduino-libraries/SD/tree/master/examples)

¡Esperamos que esta guía te sea útil en tus proyectos de Arduino! No dudes en experimentar con la creación de archivos temporales y compartir tus resultados con la comunidad. ¡Diviértete creando!