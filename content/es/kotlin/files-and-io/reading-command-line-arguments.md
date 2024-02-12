---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:56:38.101512-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer argumentos de la línea de comandos permite a tu programa en Kotlin recibir datos externos al ejecutarse. Los programadores usan esto para personalizar la ejecución sin cambiar el código.

## Cómo:
Para acceder a los argumentos de línea de comandos en Kotlin, usamos la función `main` que toma un array de Strings como argumento. Mira el ejemplo:

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hola, ${args[0]}!")
    } else {
        println("Hola, desconocido. Por favor, pasa un argumento.")
    }
}
```
Si ejecutas este programa así:

```shell
kotlin MiProgramaKt Buenos Aires
```
La salida será:
```
Hola, Buenos Aires!
```

## Inmersión Profunda:
Históricamente, leer argumentos de línea de comandos es una práctica común en la programación, especialmente en aplicaciones de consola. Kotlin hereda esta capacidad de Java, construido para ser compatible con ella. Alternativas incluyen la entrada de usuario durante la ejecución o la configuración de archivos. A nivel de implementación, cuando se pasa una lista de argumentos, Kotlin los recibe en un array que puede manejar como cualquier otro arreglo en el lenguaje.

## Ver También:
- [Kotlinlang.org - Argumentos de funciones](https://kotlinlang.org/docs/functions.html#parameters)
- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/command-line.html)
- [Tutorial de Kotlin Basic Syntax](https://kotlinlang.org/docs/basic-syntax.html)