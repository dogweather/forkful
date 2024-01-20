---
title:                "Verificando si un directorio existe"
html_title:           "Gleam: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Verificar si un directorio existe es una operación común de IO: Un programa de computadora verifica si un cierto directorio está presente en el sistema de archivos. Los programadores lo hacen para evitar errores al tratar de acceder o manipular un directorio que puede no estar allí.

## Cómo hacer:

En Gleam, podemos usar la función `directory_exists` del módulo `gleam/filesystem` para comprobar si un directorio existe. Aquí hay un ejemplo:

```Gleam
import gleam/filesystem.{directory_exists}

let dir = "/path/to/directory" 

match directory_exists(dir) {
  Ok(True) -> 
    print("El directorio existe")
  Ok(False) -> 
    print("El directorio no existe")
  Error(_) -> 
    print("Ocurrió un error al verificar el directorio")
}
```

Cuando ejecutas este código, verás una de las tres salidas, dependiendo de si el directorio existe, no existe o si ocurrió un error.

## Profundización

Históricamente, la verificación de la existencia de un directorio es un problema de programación fundamentada desde los primeros días de los sistemas operativos. Las alternativas a `directory_exists` pueden ser intentar abrir el directorio y manejar cualquier error, aunque esta es generalmente una solución menos preferida debido a su complejidad.

La función `directory_exists` en Gleam trabaja a un nivel más bajo. Utiliza llamadas al sistema operativo para preguntar directamente si un directorio está presente, lo que hace que sea una opción más eficiente y segura.

## Ver También

Para información adicional y fuentes relacionadas, consulte los siguientes enlaces:

1. [Documentación oficial de Gleam](https://gleam.run/)