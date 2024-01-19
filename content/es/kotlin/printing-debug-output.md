---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La impresión de salida de depuración en la programación es donde los desarrolladores generan mensajes útiles para rastrear el flujo y los problemas en su código. Lo hacemos para entender y solucionar rápidamente los problemas durante el desarrollo.

## Cómo hacerlo:

En Kotlin, puedes imprimir en la consola con las funciones `print()` o `println()`. Estas funciones pueden tomar cualquier tipo de valor:

```Kotlin
fun main() {  
    val nombre = "Juan"
    println("Hola, $nombre")
}
```
La salida será: `Hola, Juan`.

El registro de depuración se usa frecuentemente con la instrucción `println`. Así que una depuración puede parecerse a esto:

```Kotlin 
fun main() {
   val x = 10
   println("El valor de x es $x")
}
```
Por lo que la salida será: `El valor de x es 10`.

## Profundizando:

Históricamente, imprimir salida de depuración es una técnica antigua que usan los programadores desde los primeros días de la codificación. Ha sido invaluable para rastrear errores y seguir el flujo del programa.

En Kotlin, además de `print()` y `println()`, puedes usar la biblioteca de Logging para la depuración, que proporcionará más detalles y permitirá un filtrado más efectivo en grandes bases de código.

La implementación de la impresión de la salida de depuración es sencilla en Kotlin. Ambas funciones, `print()` y `println()`, están incorporadas en la biblioteca estándar de Kotlin, y se pueden invocar en cualquier lugar dentro de tu código.

## Ver también:

Para profundizar más sobre este tema y prácticas similares, puedes visitar los siguientes enlaces:

1. Manual oficial de Kotlin (en inglés): https://kotlinlang.org/docs/reference/basic-syntax.html#printing-to-the-console
2. Tutorial para principiantes de Kotlin (en inglés): https://developer.android.com/courses/kotlin-bootcamp/
3. Documentación sobre la biblioteca de Logging en Kotlin (en inglés): https://github.com/MicroUtils/kotlin-logging