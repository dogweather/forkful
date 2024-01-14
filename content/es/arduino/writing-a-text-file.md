---
title:                "Arduino: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto con Arduino?

Escribir un archivo de texto con Arduino puede ser útil para almacenar datos o para crear un registro de eventos. También puede ser una forma de comunicarse con un usuario final, ya que el contenido puede ser leído en un dispositivo externo.

## Cómo hacerlo

Para escribir un archivo de texto con Arduino, se pueden seguir los siguientes pasos:

1. Inicializar una variable de tipo `File` para almacenar el archivo. Ejemplo:
```Arduino
File miArchivo;
```
2. Abrir el archivo utilizando el método `open()`. Este método recibe dos parámetros: el nombre del archivo y el modo en el que se va a abrir, en este caso "w" para escribir. Ejemplo:
```Arduino
miArchivo.open("datos.txt", "w");
```
3. Utilizar el método `println()` para escribir el contenido en el archivo. Ejemplo:
```Arduino
miArchivo.println("Hola mundo!");
```
4. Cerrar el archivo con el método `close()`. Ejemplo:
```Arduino
miArchivo.close();
```

Una vez que se hayan seguido estos pasos, se tendrá un archivo de texto llamado "datos.txt" con el contenido "Hola mundo!".

## Profundizando

Existen varias funciones adicionales que se pueden utilizar para escribir un archivo de texto con Arduino, como `write()`, `print()`, `writeBytes()` y `printBytes()`. Cada una de estas tiene un propósito distinto y permite escribir diferentes tipos de datos en el archivo.

Además, es importante tener en cuenta que al escribir un archivo de texto con Arduino se debe tener en cuenta el tamaño máximo del archivo, ya que la memoria de Arduino es limitada.

## Ver también

- [Tutorial: Escribiendo archivos de texto con Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Documentación de la librería `File`](https://www.arduino.cc/reference/en/libraries/sd/file/)
- [Video: Escribir y leer archivos de texto con Arduino](https://www.youtube.com/watch?v=tRwSmT9pWVQ)