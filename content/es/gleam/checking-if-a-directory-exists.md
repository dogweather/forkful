---
title:    "Gleam: Comprobando si existe un directorio"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe en Gleam?

La verificación de la existencia de un directorio es una tarea común en la programación, especialmente en el desarrollo de aplicaciones que manejan archivos y directorios. En Gleam, esta tarea es especialmente importante debido a su enfoque en el rendimiento y la seguridad, lo que significa que siempre es recomendable verificar la existencia de un directorio antes de realizar cualquier operación en él.

## Cómo hacerlo

Para comprobar si un directorio existe en Gleam, podemos utilizar la función `os.path.exists()` de la biblioteca estándar de Gleam. Esta función toma como argumento una cadena que representa la ruta del directorio que deseamos verificar y devuelve un valor booleano que indica si el directorio existe o no. Veamos un ejemplo:

```Gleam
import os

exists := os.path.exists("/ruta/del/directorio")

if exists {
  io.println("El directorio existe.")
} else {
  io.println("El directorio no existe.")
}
```

La salida de este código variará dependiendo de si el directorio existe o no en la ruta especificada. Si el directorio existe, obtendremos el mensaje "El directorio existe", de lo contrario, obtendremos el mensaje "El directorio no existe".

## Profundizando en la verificación de la existencia de un directorio

Además de la función `os.path.exists()`, Gleam también proporciona una variedad de funciones y tipos para trabajar con archivos y directorios, como `os.path.is_dir()` para verificar si un camino dado es un directorio o `os.path.list_dir()` para obtener una lista de archivos y subdirectorios en un directorio. Estas herramientas pueden ser útiles en situaciones más complejas que requieran una inspección más profunda de los directorios.

## Ver también

- Documentación oficial de la biblioteca estándar de Gleam sobre archivos y directorios: [https://gleam.run/modules/io.os.path.html](https://gleam.run/modules/io.os.path.html)
- Ejemplos de uso de funciones relacionadas en Gleam Cookbook: [https://github.com/gleam-lang/gleam_cookbook/tree/master/01.%20File%20system](https://github.com/gleam-lang/gleam_cookbook/tree/master/01.%20File%20system) 
- Tutorial sobre el manejo de archivos y directorios en Gleam: [https://gleam.run/book/tour/file-system.html](https://gleam.run/book/tour/file-system.html)