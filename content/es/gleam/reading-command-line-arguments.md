---
title:                "Leyendo argumentos de línea de comando."
html_title:           "Gleam: Leyendo argumentos de línea de comando."
simple_title:         "Leyendo argumentos de línea de comando."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Leer argumentos de línea de comando es una forma de obtener información ingresada por el usuario en la línea de comandos de una aplicación. Los programadores lo hacen para permitir que los usuarios personalicen la ejecución de su programa, pasándole información específica al inicio.

## Cómo:
Para leer argumentos de línea de comando en Gleam, podemos usar la biblioteca estándar ```gleam/io``` que proporciona la función ```io.args```. Por ejemplo, si queremos pasar el nombre de un archivo al ejecutar nuestro programa, podemos hacerlo de la siguiente manera:

```gleam
import gleam/io

fn main(args) {
  file_name = case args {
    []       -> "archivo.txt"
    [arg]    -> arg
    _        -> "error"
  }
  // Hacemos algo con el nombre del archivo...
}
```

La función ```io.args``` devuelve una lista de argumentos de tipo cadena, que podemos analizar usando la función de coincidencia de patrones ```case```. En el caso de que se inserten varios argumentos, podemos acceder a ellos usando patrones más complejos en nuestra función ```case```.

## Deep Dive:
La práctica de leer argumentos de línea de comando es común en muchos lenguajes de programación y se remonta a los primeros días de las interfaces de línea de comandos. En Gleam, podemos usar la biblioteca estándar ```gleam/io``` para leer argumentos de línea de comando, pero también existen otras alternativas como la biblioteca ```gleam/cli```. Esta biblioteca proporciona una forma más estructurada de manejar argumentos complejos con opciones y valores predeterminados.

## See Also:
- Documentación de Gleam para la biblioteca ```gleam/io```: https://gleam.run/lib/gleam/io
- Documentación de Gleam para la biblioteca ```gleam/cli```: https://gleam.run/lib/gleam/cli