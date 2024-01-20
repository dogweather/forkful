---
title:                "Comprobando si existe un directorio"
html_title:           "Bash: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Verificar si un directorio existe es comprobar si una carpeta específica está presente en la tarjeta SD de tu dispositivo Arduino. Los programadores hacen esto para evitar errores al intentar acceder a archivos en directorios que no existen o para crear dinámicamente nuevos directorios según sea necesario.

## Cómo Hacerlo:

La librería `SD` de Arduino te permite interactuar con archivos y directorios en una tarjeta SD. Aquí está el código para comprobar si un directorio existe:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // espera que la conexión serial esté activa
  }

  if (!SD.begin(4)) {
    Serial.println("La inicialización de la SD falló!");
    return;
  }

  File dir = SD.open("/miDirectorio");
  if (dir && dir.isDirectory()) {
    Serial.println("El directorio existe!");
  } else {
    Serial.println("El directorio no existe.");
  }
  dir.close();
}

void loop() {
  // nada aquí
}
```
**Salida de muestra:**
```
El directorio existe!
```
o
```
El directorio no existe.
```

## Inmersión Profunda:

Históricamente, la verificación de un directorio ha sido una tarea esencial en la programación de sistemas y no es diferente en los entornos de Arduino. Originalmente, la funcionalidad de los sistemas de archivos en Arduino era bastante limitada, pero con la evolución de las librerías como la `SD.h`, ahora puedes hacer casi cualquier operación de archivo que podrías hacer en un sistema operativo de escritorio.

Alternativamente, podrías usar la librería `SPI.h` junto con `SD.h` para un control más bajo nivel, pero para la mayoría de las tareas, `SD.h` es más que suficiente.

Detalles de implementación a tener en cuenta:
- Asegúrate de que el pin CS (chip select) en `SD.begin(4)` coincida con el pin que estás utilizando en tu hardware.
- El objeto `File` sirve para representar tanto archivos como directorios. La función `isDirectory()` determina si el objeto `File` abierto es un directorio.
- Llamar a `dir.close()` es importante para liberar recursos del sistema.

## Ver También:

- Documentación de la librería SD: https://www.arduino.cc/en/Reference/SD
- Tutorial de Arduino sobre la manipulación de archivos: https://www.arduino.cc/en/Tutorial/LibraryExamples/Files
- Ejemplos de código y proyectos relacionados: https://create.arduino.cc/projecthub

Al trabajar con sistemas de archivos en Arduino, la práctica y experimentación es clave. Usa estos recursos como punto de partida para efectuar tus propias creaciones.