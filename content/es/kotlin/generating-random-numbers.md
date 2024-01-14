---
title:                "Kotlin: Generando números aleatorios"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Kotlin

Generar números aleatorios es una tarea común en la programación, ya sea para realizar pruebas, juegos o para experimentar con algoritmos. En Kotlin, hay varias formas de generar números aleatorios y en esta publicación de blog te explicaremos cómo hacerlo.

## Cómo generar números aleatorios en Kotlin

Para generar números aleatorios en Kotlin, podemos utilizar la clase `Random` y sus métodos `nextInt()` y `nextDouble()`. Aquí hay un ejemplo de código que muestra cómo generar un número entero aleatorio entre 0 y 10:

```Kotlin
val random = Random()
val randomNumber = random.nextInt(10)
println("Número aleatorio: $randomNumber")
```

El código primero crea una instancia de la clase `Random` y luego utiliza el método `nextInt()` para generar un número aleatorio entre 0 (incluido) y 10 (excluido). En este caso, este número se almacena en la variable `randomNumber` y se imprime en la consola.

También podemos utilizar el método `nextDouble()` para generar números decimales entre 0.0 (incluido) y 1.0 (excluido). Aquí hay un ejemplo de código que muestra cómo generar un número decimal aleatorio:

```Kotlin
val random = Random()
val randomDecimal = random.nextDouble()
println("Número aleatorio: $randomDecimal")
```

## Profundizando en la generación de números aleatorios

La clase `Random` en Kotlin utiliza un algoritmo de generación de números pseudoaleatorios. Esto significa que los números generados no son realmente aleatorios, sino que siguen un patrón predecible. Sin embargo, este es un método comúnmente utilizado y es suficiente para muchas aplicaciones.

Un punto importante a tener en cuenta al generar números aleatorios en Kotlin es que siempre debemos crear una nueva instancia de la clase `Random` cada vez que la usamos. Esto se debe a que si utilizamos la misma instancia varias veces, los números generados seguirán un patrón predecible.

## Ver también

- [Documentación oficial de Kotlin sobre generación de números aleatorios](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Ejemplo de generación de números aleatorios en Kotlin](https://www.woolha.com/tutorials/kotlin-generate-a-random-number)