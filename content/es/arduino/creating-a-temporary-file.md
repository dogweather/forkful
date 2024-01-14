---
title:    "Arduino: Creando un archivo temporal"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido almacenar datos temporales en tu proyecto de Arduino? Si es así, crear un archivo temporal podría ser la solución perfecta para ti. Un archivo temporal es un archivo que se crea y se utiliza temporalmente para almacenar datos que no necesitas de forma permanente. Esto puede ser útil en situaciones en las que necesitas almacenar y recuperar datos de forma rápida y eficiente sin tener que preocuparte por guardarlos permanentemente en la memoria del dispositivo.

## Cómo hacerlo

Para crear un archivo temporal en Arduino, necesitarás utilizar la biblioteca `SD.h`. Esta biblioteca permite acceder a una tarjeta SD para almacenar y recuperar datos. Aquí hay un ejemplo de código que puedes utilizar para crear un archivo temporal y escribir datos en él:

```
#include <SD.h>
File tempFile;

void setup() {
  // Inicializar la tarjeta SD
  SD.begin(10);

  // Crear un archivo temporal llamado "temp.txt"
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  // Escribir datos en el archivo
  tempFile.println("¡Hola, mundo!");
  tempFile.println("Esto es un archivo temporal.");
  
  // Cerrar el archivo
  tempFile.close();
}

void loop() {
  // Código de tu proyecto
}
```

Este código crea un archivo temporal llamado "temp.txt" y escribe dos líneas de texto dentro de él. Luego, cuando se cierra el archivo, la tarjeta SD se puede utilizar para otras tareas. Cuando necesites acceder a los datos almacenados en el archivo temporal, simplemente puedes abrirlo y leerlos.

## Profundizando

Crear un archivo temporal puede ser muy útil para proyectos que requieren almacenamiento temporal de datos, como registros o logs. Sin embargo, es importante tener en cuenta que los archivos temporales se eliminarán automáticamente cuando se reinicie el dispositivo o se apague la alimentación. Por lo tanto, no deben utilizarse para almacenar datos importantes o permanentes.

Si deseas eliminar manualmente un archivo temporal, puedes hacerlo utilizando el método `remove()` de la clase `SD`. Este método eliminará el archivo especificado de la tarjeta SD.

## Ver también

- [Biblioteca SD.h en Arduino](https://www.arduino.cc/en/Reference/SD)
- [Cifrar archivos temporales en Arduino](https://arduinoetcetera.wordpress.com/2015/02/01/cifrar-archivos-temporales-arduino-sdhc/)
- [Escribir y leer datos en una tarjeta SD con Arduino](https://www.luisllamas.es/escribir-leer-datos-tarjeta-sd-arduino/)