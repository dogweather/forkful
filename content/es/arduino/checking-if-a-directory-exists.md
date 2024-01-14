---
title:                "Arduino: Comprobando si existe un directorio"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En una aplicación de Arduino, puede ser útil verificar si un directorio existe antes de realizar ciertas operaciones, como guardar o leer archivos. Esto puede ayudar a evitar errores y a mejorar la eficiencia de nuestro código.

## Cómo hacerlo

Para verificar si un directorio existe en Arduino, podemos utilizar el método `exists()` de la librería `SD` (SD card library). Este método retorna `true` si el directorio existe y `false` si no existe. A continuación, un ejemplo de código que muestra cómo utilizarlo:

```Arduino
#include <SD.h> // Incluimos la librería

void setup() {
  // Inicializamos la comunicación con la tarjeta SD
  while(!Serial) { // Esperamos a que se establezca la comunicación serial
    ; // No hacemos nada
  }

  Serial.begin(9600); // Iniciamos la comunicación serial
  
  // Verificar si el directorio "archivos" existe
  if(SD.exists("archivos")) { // Si existe
    Serial.println("El directorio existe.");
  } else { // Si no existe
    Serial.println("El directorio no existe.");
  }
}

void loop() {

}
```

Si ejecutamos este código y el directorio "archivos" existe, el resultado en el monitor serial sería:

```
El directorio existe.
```

Si el directorio no existe, el resultado sería:

```
El directorio no existe.
```

## Profundizando

El método `exists()` de la librería `SD` utiliza una estructura de directorios en árbol para determinar si un directorio existe o no. Esta estructura de directorios se basa en el formato FAT16/FAT32 utilizado en la mayoría de las tarjetas SD. Básicamente, el método recorre el árbol de directorios buscando el nombre del directorio específico que se le ha pasado como parámetro.

Una cosa importante a tener en cuenta es que este método solo funciona para verificar la existencia de directorios en la raíz de la tarjeta SD. Si deseamos verificar un directorio en una subcarpeta, debemos agregar el nombre de la subcarpeta al directorio que pasamos como parámetro. Por ejemplo, si queremos verificar si el directorio "subcarpeta" existe en la carpeta "archivos", deberíamos escribir `SD.exists("archivos/subcarpeta")`.

## Ver también

- [Documentación oficial de la librería SD](https://www.arduino.cc/reference/en/libraries/sd/)
- [Tutorial: Cómo utilizar una tarjeta SD con Arduino](https://www.prometec.net/tarjeta-sd-arduino/)