---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Gleam: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si has estado programando en línea de comandos, probablemente hayas notado que a menudo puedes pasar argumentos al programa que estás ejecutando. Estos argumentos pueden ser útiles para personalizar la ejecución de tu programa, y en este artículo te explicaremos cómo leer y utilizar estos argumentos en tus programas escritos en Gleam.

## ¿Cómo hacerlo?

Para leer los argumentos de línea de comandos en Gleam, utilizaremos la función `Gleam.App.args()` que nos devuelve una lista de cadenas de texto con los argumentos pasados al programa. A continuación, podemos utilizar la función `Gleam.List.get(idx: Int, list: list(a))` para obtener un argumento en particular a través de su índice en la lista.

Veamos un ejemplo con un programa que acepta un nombre de usuario como argumento y lo imprime por pantalla:

```Gleam
import Gleam.App

fn main() {
  // Obtenemos una lista de argumentos
  let args = Gleam.App.args()

  // Obtenemos el primer argumento
  let nombre = List.get(0, args)

  // Imprimimos un saludo utilizando el argumento
  Gleam.IO.print("Hola, #{nombre}!")
}
```

Si ejecutamos este programa con el comando `Gleam run programa.gleam Juan`, el resultado sería `Hola, Juan!`.

## Profundizando

La función `Gleam.App.args()` en realidad utiliza una llamada al sistema operativo para obtener los argumentos de línea de comandos, lo cual nos permite leer argumentos más complejos como banderas o opciones. También podemos utilizar la biblioteca `Gleam.OS.ArgParser` para analizar argumentos de línea de comandos de una manera más estructurada.

## Ver también

- [Documentación de Gleam sobre la función `Gleam.App.args()`](https://gleam.run/book/std.types.html#Gleam.App.args)
- [Documentación de Gleam sobre la biblioteca `Gleam.OS.ArgParser`](https://gleam.run/book/stdlib.html#Gleam.OS.ArgParser)