---
title:                "Generando números aleatorios"
html_title:           "Kotlin: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Generar números aleatorios es una habilidad importante en la programación ya que permite crear programas más dinámicos y divertidos. Además, puede ser útil para aplicaciones de juegos, criptografía, pruebas de software y más.

## Cómo hacerlo

Para generar números aleatorios en Kotlin, primero debemos importar la clase `Random` del paquete `kotlin.random`. Luego, podemos llamar al método `nextInt()` para obtener un número aleatorio de tipo entero. Aquí hay un ejemplo de código que genera cinco números aleatorios y los imprime en la consola:

```Kotlin
import kotlin.random.Random

fun main() {
    for (i in 1..5) {
        val number = Random.nextInt()
        println(number)
    }
}

// Salida de ejemplo:
// 18237236
// -456896
// 907312
// -982347
// 6207354
```

También podemos especificar un rango para los números aleatorios utilizando el método `nextInt(range)`, donde `range` es un rango de valores entre los cuales queremos generar el número aleatorio. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos usar `Random.nextInt(1..10)`.

Además, también podemos especificar el tipo de datos que queremos para el número aleatorio, como `nextBoolean()` para un valor booleano, `nextDouble()` para un número de punto flotante o `nextBytes()` para un arreglo de bytes aleatorios.

## Profundizando en generar números aleatorios

La generación de números aleatorios se basa en algoritmos que utilizan semillas para producir una secuencia de números aparentemente aleatorios. En Kotlin, la clase `Random` utiliza el algoritmo Mersenne Twister para generar números aleatorios, que es un algoritmo ampliamente utilizado y probado.

Sin embargo, es importante tener en cuenta que los números generados no son realmente aleatorios, sino que se obtienen de una secuencia determinística. Esto significa que si ejecutamos el mismo código varias veces, obtendremos la misma secuencia de números aleatorios.

También podemos utilizar semillas para controlar la secuencia de números aleatorios que se generan. Esto puede ser útil para fines de pruebas o para obtener la misma secuencia de números aleatorios en diferentes ejecuciones del programa.

## Véase también

- [Documentación oficial de Random en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Tutorial sobre generación de números aleatorios en Kotlin](https://developer.android.com/kotlin/controls/random)
- [Mersenne Twister en Wikipedia (en inglés)](https://en.wikipedia.org/wiki/Mersenne_Twister)