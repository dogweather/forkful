---
title:                "Escribiendo un archivo de texto"
aliases:
- es/arduino/writing-a-text-file.md
date:                  2024-02-03T19:26:51.881896-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo un archivo de texto"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto en Arduino implica guardar datos en un archivo en una tarjeta SD o un módulo de almacenamiento similar, a menudo para fines de registro de datos. Los programadores hacen esto para registrar lecturas de sensores, guardar configuraciones o registrar eventos de aplicaciones a lo largo del tiempo, lo que lo hace crucial para proyectos que requieren análisis de datos o seguimiento.

## Cómo:
Para escribir en un archivo de texto en una tarjeta SD usando Arduino, primero necesitas incluir la biblioteca `SD.h`, que proporciona las funciones necesarias para interactuar con tarjetas SD. Asegúrate de que tu placa Arduino esté conectada a un módulo de tarjeta SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Inicia la comunicación serial a 9600 bits por segundo:
  Serial.begin(9600);
  
  // Verifica la inicialización de la tarjeta SD
  if (!SD.begin(4)) {
    Serial.println("¡Inicialización fallida!");
    return;
  }
  Serial.println("Inicialización completada.");
  
  // Abre el archivo. Nota que solo se puede abrir un archivo a la vez,
  // así que tienes que cerrar este antes de abrir otro.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Si el archivo se abrió correctamente, escribe en él:
  if (myFile) {
    Serial.print("Escribiendo en test.txt...");
    myFile.println("Probando escritura de archivo de texto.");
    // Cierra el archivo:
    myFile.close();
    Serial.println("hecho.");
  } else {
    // Si el archivo no se abrió, imprime un error:
    Serial.println("Error al abrir test.txt");
  }
}

void loop() {
  // Nada sucede después del setup
}
```

### Salida de Ejemplo:
Cuando ejecutas este código, el Monitor Serie de Arduino IDE mostrará:
```
Inicialización completada.
Escribiendo en test.txt...hecho.
```
Para verificar si los datos se escribieron correctamente, puedes quitar la tarjeta SD del Arduino, insertarla en una computadora y abrir el archivo `test.txt` para ver el mensaje "Probando escritura de archivo de texto."

Para proyectos que requieren operaciones de archivos más avanzadas o procesamiento, considera explorar bibliotecas adicionales o escribir funciones personalizadas adaptadas a tus necesidades específicas.
