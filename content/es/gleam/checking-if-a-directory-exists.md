---
title:                "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Cuando estás escribiendo un programa en Gleam, es importante asegurarte de que tu código sea robusto y pueda manejar todas las posibles situaciones. Una de estas situaciones es verificar si un directorio existe antes de intentar acceder a él. En esta publicación, aprenderemos cómo hacer eso usando el lenguaje de programación Gleam.

## Cómo hacerlo

Comencemos por importar el módulo `os` que nos permitirá acceder a las funciones del sistema operativo.

```Gleam
import os
```

A continuación, definiremos una función llamada `check_directory` que tomará como argumento una ruta de directorio y devolverá un valor `bool` indicando si el directorio existe o no.

```Gleam
pub fn check_directory(directory) {
  let result = os.dir_exists(directory)
  result
}
```

Luego, podemos llamar a esta función y pasarle una ruta de directorio como parámetro. Aquí hay un ejemplo donde intentamos verificar si el directorio "docs" existe en la carpeta actual y luego imprimimos un mensaje en consecuencia.

```Gleam
let directory = "./docs"
let exists = check_directory(directory)
if exists {
  println("¡El directorio existe!")
} else {
  println("El directorio no existe.")
}
```

Si ejecutamos este código, obtendremos el siguiente resultado:

```
¡El directorio existe!
```

## Profundizando

Ahora que sabemos cómo verificar si un directorio existe en Gleam, es importante entender cómo funciona esta función. El módulo `os` utiliza funciones de bajo nivel proporcionadas por el sistema operativo para acceder a los archivos y directorios del sistema. El método `dir_exists` se comunica con el sistema operativo para comprobar si el directorio especificado existe o no. Si el directorio existe, devuelve un valor `true` y si no, devuelve un valor `false`.

## Ver también

- [Documentación del módulo Gleam os](https://gleam.run/modules/standard-lib/os/)
- [Funciones del sistema operativo en Gleam](https://gleam.run/book/stdlib-filesystem#operating-system-functions)