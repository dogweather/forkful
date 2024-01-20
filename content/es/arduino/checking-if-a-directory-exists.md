---
title:                "Verificando si un directorio existe"
html_title:           "Arduino: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Verificar si un directorio existe es un procedimiento común en la programación que nos ayuda a evitar errores en tiempo de ejecución. Lo hacemos para validar la existencia de un directorio antes de intentar operaciones como abrir, leer o escribir en el directorio.

## Cómo hacerlo

Para comprobar si un directorio existe en Arduino, podemos usar los comandos de la biblioteca SD. Aquí te dejo un ejemplo simple:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // Esperar a que el puerto serie esté disponible
  }

  if (!SD.begin(4)) {
    Serial.println("La inicialización ha fallado!");
    while (1);
  } else {
    Serial.println("La inicialización ha tenido éxito!");
    if (SD.exists("/example")) {
      Serial.println("El directorio existe.");
    } else {
      Serial.println("El directorio no existe.");
    }
  }
}
```

En este ejemplo, el código comprobará si el directorio '/example' existe en la tarjeta SD inicializada en el pin 4.

## Inmersión Profunda

A lo largo de la historia de la programación, la necesidad de comprobar si un directorio existe ha sido una constante. Antes de la creación de bibliotecas como SD para Arduino, los programadores tenían que escribir largas funciones en bajo nivel para hacer lo mismo.

Como alternativa a la comprobación de directorios, podrías intentar abrir o escribir en un directorio sin verificar su existencia, pero esto podría llevar a fallos en el programa si el directorio no existe.

En cuanto a la implementación, la función SD.exists utiliza debajo de su capa los comandos fopen y fclose de C para chequear la existencia del directorio. Esta es una práctica común en otros sistemas de archivos también.

## Ver También

Puedes encontrar más información y ejemplos de código sobre la biblioteca SD para Arduino en los siguientes enlaces:

- [Documentación de la Biblioteca SD](https://www.arduino.cc/en/Reference/SD)
- [Foro de Arduino - Dudas y discusiones acerca de programación con Arduino](https://forum.arduino.cc/index.php?topic=63667.0)