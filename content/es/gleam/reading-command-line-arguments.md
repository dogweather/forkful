---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer argumentos de línea de comandos es el proceso por el cual los programas obtienen datos de entrada directamente desde la línea de comandos del usuario. Los programadores hacen esto para permitir que sus programas se ejecuten de manera personalizada al proporcionar una interfaz flexible para introducir los datos.

## Cómo Hacerlo:

En Gleam, leemos argumentos de línea de comandos utilizando el módulo `os.args`. Aquí hay un código de muestra:

```Gleam
import gleam/os

/// Una función que imprime los argumentos de línea de comandos del usuario.
pub fn main(args: List(String)) {
  let user_args = os.args
  case user_args {
    [] -> "No se proporcionaron argumentos!".println()
    [first | rest] -> {
      "El primer argumento es:".println()
      first.println()
      "Los argumentos restantes son:".println()
      rest.println()
    }
  }
}
```

Si ejecuta este programa con `gleam run main arg1 arg2 arg3` , obtendrá un resultado similar a esto:

```Gleam
El primer argumento es:
arg1
Los argumentos restantes son:
[arg2, arg3]
```

## Análisis Más Profundo:

Los argumentos de línea de comandos han sido una interfaz estándar para interactuar con los programas desde los primeros días de los sistemas operativos de texto. Son fundamentales para permitir que los usuarios pasen parámetros a un programa al momento de su ejecución.

Como alternativa a los argumentos de línea de comandos, algunos programas pueden optar por leer archivos de entrada, pedir entrada del usuario durante la ejecución o incluso capturar señales del sistema.

En cuanto a la implementación, Gleam usa la interfaz de línea de comandos del sistema operativo. Cuando el comando se ejecuta, los argumentos de la línea de comandos se pasan al programa como una lista de cadenas.

## Ver También:

1. [Documentación oficial de Gleam](https://gleam.run/): Para obtener más información sobre Gleam y cómo funciona.