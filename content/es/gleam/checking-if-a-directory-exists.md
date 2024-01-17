---
title:                "Comprobando si existe un directorio"
html_title:           "Gleam: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comprobar si un directorio existe es una acción común en la programación. Consiste en verificar si una carpeta o directorio específico está presente en el sistema de archivos. Los programadores suelen hacerlo para asegurarse de que el código funcione correctamente o para realizar acciones dependiendo de si el directorio existe o no.

## Cómo hacerlo:

Usando Gleam, podemos comprobar si un directorio existe utilizando la función `OS.exists` y pasándole como argumento el directorio que queremos verificar. A continuación, se muestra un ejemplo de código y su posible resultado:

```Gleam
let directorio = "mi/carpeta"
let existe = OS.exists(directorio)
```
```
# resultado: `True` o `False`
```

## Inmersión profunda:

Antes de la existencia de sistemas de archivos, no había necesidad de comprobar si un directorio existía, ya que simplemente se asumía que estaba presente. Hoy en día, existen diferentes formas de implementar esta comprobación, como utilizar comandos del sistema operativo o bibliotecas específicas de lenguaje de programación.

## Ver también:

Para más información sobre cómo trabajar con el sistema de archivos en Gleam, puedes consultar la [documentación oficial](https://gleam.run/book/standard-library.html#file-system). También puedes explorar otras fuentes para aprender sobre la importancia de comprobar la existencia de un directorio en la programación.