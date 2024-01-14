---
title:                "Gleam: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar argumentos de línea de comando en programación?

Los argumentos de línea de comando son una herramienta valiosa en la programación. Nos permiten interactuar con nuestro código y personalizar su comportamiento sin necesidad de modificarlo directamente. Además, su uso facilita la ejecución de nuestro programa y puede ahorrar tiempo en tareas repetitivas.

## Cómo utilizar argumentos de línea de comando en Gleam

En Gleam, podemos acceder a los argumentos de línea de comando con la función `gleam/core:os.args()`. Esta función devuelve una lista con todos los argumentos especificados al momento de ejecutar nuestro programa.

Veamos un ejemplo de cómo podemos utilizar esta función en un programa de Gleam:

```
Gleam import os.io

pub fn main(args: List(String)) {
  if len(args) > 1 {
    io.print("Hola, " ++ args[1])
  } else {
    io.print("Hola, mundo!")
  }
}
```

Al ejecutar este programa con el siguiente comando `gleam run hola.gleam Juan`, obtendremos como resultado "Hola, Juan".

## Profundizando en la lectura de argumentos de línea de comando

Además de la función `os.args()`, también podemos utilizar los módulos `gleam/args` y `gleam/parse` para trabajar con argumentos de línea de comando de manera más avanzada. Estos módulos nos permiten definir opciones y valores esperados en nuestro código, lo que brinda una mayor estructura y flexibilidad al momento de leer argumentos.

Por ejemplo, con el módulo `gleam/args` podemos definir opciones que pueden ser activadas o desactivadas mediante la línea de comando, mientras que con el módulo `gleam/parse` podemos especificar qué tipo de valor esperamos recibir para cada argumento.

## Ver también

- Documentación oficial de Gleam sobre argumentos de línea de comando: [https://gleam.run/book/tour/command_line_arguments.html](https://gleam.run/book/tour/command_line_arguments.html)
- Ejemplos de manipulación de argumentos de línea de comando en Gleam: [https://github.com/gleam-lang/gleam/blob/master/examples/cli/](https://github.com/gleam-lang/gleam/blob/master/examples/cli/)
- Tutorial de Gleam sobre lectura de argumentos en línea de comando: [https://gleam.run/book/tutorials/commands.html](https://gleam.run/book/tutorials/commands.html)