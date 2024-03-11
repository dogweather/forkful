---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:18.474053-07:00
description: "Escribir en el error est\xE1ndar (stderr) en la programaci\xF3n de Arduino\
  \ implica dirigir los mensajes de error y diagn\xF3sticos a un canal separado, asegurando\u2026"
lastmod: '2024-03-11T00:14:33.172353-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) en la programaci\xF3n de Arduino\
  \ implica dirigir los mensajes de error y diagn\xF3sticos a un canal separado, asegurando\u2026"
title: "Escribiendo en el error est\xE1ndar"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir en el error estándar (stderr) en la programación de Arduino implica dirigir los mensajes de error y diagnósticos a un canal separado, asegurando que no se mezclen con la salida estándar (stdout). Los programadores hacen esto para diferenciar las salidas normales del programa de los mensajes de error, facilitando la depuración y el análisis de registros.

## Cómo hacerlo:

Arduino no diferencia de forma nativa entre la salida estándar y el error estándar como lo hacen los sistemas informáticos convencionales. Los métodos `Serial.print()` y `Serial.println()` escriben en la misma salida serial, que típicamente se visualiza en el Monitor Serial del IDE de Arduino. Sin embargo, podemos emular la escritura en stderr al formatear específicamente los mensajes de error o dirigiéndolos a una salida alternativa, como un archivo en una tarjeta SD o a través de una conexión de red.

Para emular stderr, puedes prefijar los mensajes de error con una etiqueta como "ERROR:" para diferenciarlos en el Monitor Serial:

```cpp
void setup() {
  Serial.begin(9600); // Inicializa la comunicación serial a 9600 baudios
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulando stderr al prefijar el mensaje de error
    Serial.println("ERROR: La función falló en ejecutarse.");
  } else {
    Serial.println("La función se ejecutó exitosamente.");
  }
  delay(1000); // Espera un segundo antes de reiniciar el bucle
}

int someFunction() {
  // Una función ficticia que retorna -1 en caso de error
  return -1;
}
```

La salida de muestra en el Monitor Serial del IDE de Arduino podría verse así:

```
ERROR: La función falló en ejecutarse.
```

Para proyectos que requieren un enfoque más sofisticado, incluyendo escribir en diferentes salidas físicas, puede ser necesario el uso de librerías de terceros o hardware adicional. Por ejemplo, registrar mensajes de error en una tarjeta SD requiere la librería `SD`:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: ¡La inicialización de la tarjeta SD falló!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: La función falló en ejecutarse.");
    myFile.close(); // Asegúrate de cerrar el archivo para guardar los contenidos
  } else {
    Serial.println("ERROR: Falló al abrir error.log!");
  }
}

void loop() {
  // Tu código principal iría aquí
}
```

Con este enfoque, físicamente separas la salida normal del programa y los mensajes de error dirigiendo estos últimos a un archivo `error.log` en una tarjeta SD, lo que permite análisis post-mortem sin saturar el canal de salida primario.
