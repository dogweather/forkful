---
title:                "Arduino: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En ocasiones, al programar en Arduino, es posible que necesitemos verificar si un directorio existe en nuestra tarjeta SD antes de realizar cualquier acción. Esto puede ser útil para evitar errores en nuestro código o tomar decisiones basadas en la existencia o no de un directorio.

## Cómo hacerlo

Para verificar si un directorio existe en nuestra tarjeta SD, podemos utilizar la función `SD.exists()` de la librería `SD.h`. Esta función devuelve un valor booleano, `true` si el directorio existe y `false` si no.

Veamos un ejemplo de cómo utilizar esta función:

```Arduino
#include <SD.h> // incluimos la librería SD.h

void setup() {
  Serial.begin(9600); // iniciamos la comunicación serial
  if(SD.begin()) { // inicializamos la tarjeta SD
    // aquí podemos realizar otras acciones si la inicialización fue exitosa
    if(SD.exists("miDirectorio")) { // verificamos si el directorio existe
      Serial.println("El directorio existe");
    } else {
      Serial.println("El directorio NO existe");
    }
  } else {
    Serial.println("No se pudo inicializar la tarjeta SD");
  }
}

void loop() {
  // no hacemos nada en el loop, solo esperamos a que el usuario revise la salida serial
}
```

La salida en el monitor serial sería:

```
El directorio existe
```

Si cambiamos el nombre del directorio por uno que no exista, la salida sería:

```
El directorio NO existe
```

## Profundizando

¿Cómo funciona la función `SD.exists()`? Esta función utiliza la función `SD.open()` de la librería `SD.h` para intentar abrir el archivo o directorio especificado. Si no se puede abrir, significa que este no existe, por lo que se devuelve `false`.

Cabe mencionar que, si bien esta función es útil para verificar la existencia de un directorio, no nos dice si este es un directorio vacío o si contiene archivos. Para realizar estas verificaciones, deberemos utilizar otras funciones de la librería `SD.h`.

## Ver también

Aquí te dejamos algunos recursos en español para seguir aprendiendo sobre el uso de la tarjeta SD en Arduino:

- [Tutorial de Arduino sobre la tarjeta SD](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Videotutorial de Código Facilito sobre manejo de archivos en SD con SD.h](https://www.youtube.com/watch?v=arVvHz1mBxQ)
- [Documentación oficial de la función SD.exists()](https://www.arduino.cc/en/Reference/SDexists)