---
title:                "Kotlin: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por Qué

La impresión de la salida de depuración es una herramienta valiosa para los programadores. Con ella, podemos ver los valores de nuestras variables y realizar un seguimiento del flujo de nuestro código. Esto puede ayudarnos a identificar posibles errores y a entender mejor lo que está sucediendo en nuestro programa.

## Cómo Hacerlo

En Kotlin, podemos imprimir la salida de depuración utilizando la función `println()`. Veamos un ejemplo de cómo imprimir una cadena de texto y el valor de una variable:

```Kotlin
val nombre = "María"

println("¡Hola, $nombre!") // Salida: ¡Hola, María!
println("El valor de nombre es $nombre") // Salida: El valor de nombre es María
```

Podemos ver que utilizamos la sintaxis `$variable` para incluir el valor de la variable en la cadena de texto. Esto nos permite imprimir valores dinámicos en nuestra salida de depuración.

También podemos utilizar la función `print()` para imprimir sin agregar una nueva línea al final. Y si queremos imprimir los valores de múltiples variables en una sola línea, podemos utilizar la función `print()` varias veces y agregar la función `println()` al final para agregar una nueva línea.

## Profundizando

Cuando imprimimos valores de variables, es importante tener en cuenta los tipos de datos. Si intentamos imprimir una variable de tipo `Int` sin convertirla a `String`, obtendremos un error. Podemos solucionar esto utilizando la función `toString()`:

```Kotlin
val num = 42

println("El número es " + num) // Error: Type mismatch
println("El número es " + num.toString()) // Salida: El número es 42
```

También podemos utilizar la función `debug()` para imprimir información de depuración más detallada, como el nombre de la función y la línea de código donde se encuentra la impresión. Esto puede ser útil para identificar rápidamente dónde se está imprimiendo la salida en nuestro código.

## Ver También

- [Documentación oficial de Kotlin sobre la función `println()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/io/println.html)
- [Tutorial de Kotlin sobre la depuración](https://kotlinlang.org/docs/tutorials/command-line.html#debugging)