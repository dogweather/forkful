---
date: 2024-01-20 17:56:38.101512-07:00
description: "Leer argumentos de la l\xEDnea de comandos permite a tu programa en\
  \ Kotlin recibir datos externos al ejecutarse. Los programadores usan esto para\u2026"
lastmod: '2024-03-13T22:44:59.051292-06:00'
model: gpt-4-1106-preview
summary: "Leer argumentos de la l\xEDnea de comandos permite a tu programa en Kotlin\
  \ recibir datos externos al ejecutarse."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

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
